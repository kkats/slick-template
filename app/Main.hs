{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where
import Control.Lens ((?~), at)
import Control.Monad (void, forM)
import Data.Aeson (FromJSON, ToJSON, Value (Object, String), toJSON)
import Data.Aeson.KeyMap (union)
import Data.Aeson.Lens (_Object)
import Data.List (sortBy)
import qualified Data.Text as T
import Data.Time
  ( UTCTime, defaultTimeLocale, getCurrentTime, parseTimeOrError)
--   , formatTime, iso8601DateFormat, parseTimeOrError)
import Development.Shake
  ( Action, Verbosity(Verbose), copyFileChanged, forP, getDirectoryFiles
  , liftIO, readFile', shakeLintInside, shakeOptions, shakeVerbosity, shakeRebuild, Rebuild (..)
  , writeFile')
import Data.Time.Format.ISO8601
import Development.Shake.Classes (Binary)
import Development.Shake.FilePath ((</>), (-<.>), dropDirectory1)
import Development.Shake.Forward (cacheAction, shakeArgsForward)
import qualified Data.HashMap.Lazy as HML
import GHC.Generics (Generic)
import Slick (compileTemplate', convert, markdownToHTML, substitute)
import Slick.Pandoc (markdownToHTMLWithOpts)
import Text.Pandoc.Options
import Text.Pandoc.Highlighting (tango)

-- import Debug.Trace (trace)

---Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta =
    SiteMeta { siteAuthor = "katsumata"
             , baseUrl = "https://www-aos.eps.s.u-tokyo.ac.jp/~katsumata"
             , siteTitle = "Ocean Circulation Research"
             , bskyHandle = Just "kkats.bsky.social"
             , githubUser = Just "kkats"
             }

outputFolder :: FilePath
outputFolder = "docs/"

--Data models-------------------------------------------------------------------

withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ union obj siteMetaObj
  where
    Object siteMetaObj = toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

data SiteMeta =
    SiteMeta { siteAuthor    :: String
             , baseUrl       :: String -- e.g. https://example.ca
             , siteTitle     :: String
             , bskyHandle :: Maybe String -- Without @
             , githubUser    :: Maybe String
             }
    deriving (Generic, Eq, Ord, Show, ToJSON)

-- | Data for the index page
data IndexInfo =
  IndexInfo
    { posts :: [Post],
      alltags :: [Tag],
      prevlist :: Bool,
      prevnum :: String,
      nextlist :: Bool,
      nextnum :: String
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | number of entries on a page of blog list
listLength :: Int
listLength = 10

data TagInfo = TagInfo { tagtag :: Tag,
                         tagposts :: [Post]
                       } deriving (Generic, Show, FromJSON, ToJSON)

type Tag = String

-- | Data for a blog post
data Post =
    Post { title       :: String
         , author      :: String
         , content     :: String
         , url         :: String
         , date        :: String
         , tags        :: [Tag]
         , description :: String
         , image       :: Maybe String
         }
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

data AtomData =
  AtomData { title        :: String
           , domain       :: String
           , author       :: String
           , posts        :: [Post]
           , currentTime  :: String
           , atomUrl      :: String } deriving (Generic, ToJSON, Eq, Ord, Show)

-- https://matsubara0507.github.io/posts/2021-06-13-my-site-use-slick.html
groupByTag :: [Post] -> [(Tag, [Post])]
groupByTag = HML.toList . foldl go mempty
  where
    go :: HML.HashMap Tag [Post] -> Post -> HML.HashMap Tag [Post]
    go acc post = foldl (\acc' tag -> HML.insertWith (++) tag [post] acc') acc (tags post)

buildTagPages :: [Post] -> Action [Tag]
buildTagPages posts' = do
    tagT <- compileTemplate' "site/templates/tag-list.html"
    forM (groupByTag posts') $ \(tag0, posts0)
                -> let tagInfo = TagInfo tag0 posts0
                       taglistHTML = T.unpack $ substitute tagT (withSiteMeta $ toJSON tagInfo)
                    in writeFile' (outputFolder </>  "tags" </> (tag0 ++ ".html")) taglistHTML
                       >> return tag0

-- | given a list of posts this will build a table of contents
buildIndex :: [Post] -> [Tag] -> Action ()
buildIndex posts' tags' = do
  postlistT <- compileTemplate' "site/templates/post-list.html"

  let buildWithPage :: Int -> [Post] -> Action ()
      buildWithPage _ [] = pure ()
      buildWithPage n post0
        | length post0 > listLength =
                let post1 = take listLength post0
                    indexInfo = if n < 1 then IndexInfo post1 tags' False "dummy" True (show $ n+1)
                                         else IndexInfo post1 tags' True (show $ n-1) True (show $ n+1)
                    postlistHTML = T.unpack $ substitute postlistT (withSiteMeta $ toJSON indexInfo)
                 in writeFile' (outputFolder </> ("posts/post-list" ++ (show n) ++ ".html")) postlistHTML
                    >> buildWithPage (n+1) (drop listLength post0)
        | otherwise = do
                let indexInfo = if n < 1 then IndexInfo post0 tags' False "dummy" True (show $ n+1)
                                         else IndexInfo post0 tags' True (show $ n-1) False "dummy"
                    postlistHTML = T.unpack $ substitute postlistT (withSiteMeta $ toJSON indexInfo)
                 in writeFile' (outputFolder </> ("posts/post-list" ++ (show n) ++ ".html")) postlistHTML

  buildWithPage 0 posts'
  
  indexT <- compileTemplate' "site/templates/index.html"
  -- let indexHTML = T.unpack $ substitute indexT (withSiteMeta $ toJSON indexInfo)
  let indexHTML = T.unpack $ substitute indexT (toJSON siteMeta)  -- no need to merge indexInfo
  writeFile' (outputFolder </> "index.html") indexHTML

buildIndexE :: Action ()
buildIndexE = do
  indexT <- compileTemplate' "site/templates/index-e.html"
  let indexHTML = T.unpack $ substitute indexT (toJSON siteMeta)
  writeFile' (outputFolder </> "index-e.html") indexHTML

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- sortBy (\a b -> b `compare` a) `fmap` getDirectoryFiles "." ["site/posts//*.md"] -- Newer to older by filename
  forP pPaths buildPost

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
--
-- local rule: If 7th character of the filename is 'M'
-- use MathJax
--
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  -- local rule
  let filenameOnly = dropDirectory1 . dropDirectory1 $ srcPath
      templateHere = if length filenameOnly > 7 && filenameOnly !! 6 == 'M'
                       then "site/templates/postWithMJ.html"
                       else "site/templates/post.html"
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob - with MathJax
  let defaultHtml5OptionsWMJ :: WriterOptions
      defaultHtml5OptionsWMJ = def {writerHighlightStyle = Just tango,
                                    writerExtensions = writerExtensions def,
                                    writerHTMLMathMethod = MathJax ""}
  let defaultMarkdownOptionsWMJ :: ReaderOptions
      defaultMarkdownOptionsWMJ = def { readerExtensions = mconcat
                                                   [extensionsFromList
                                                    [Ext_yaml_metadata_block, Ext_fenced_code_attributes,
                                                     Ext_auto_identifiers, Ext_footnotes,
                                                     Ext_tex_math_dollars, Ext_tex_math_double_backslash],       
                                                    githubMarkdownExtensions] }
  postData <- markdownToHTMLWithOpts defaultMarkdownOptionsWMJ defaultHtml5OptionsWMJ . T.pack $ postContent
  -- liftIO $ print postData
  let postUrl = T.pack . dropDirectory1 . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
  -- Add additional metadata we've been able to compute
  let fullPostData = withSiteMeta . withPostUrl $ postData
  template <- compileTemplate' templateHere -- "site/templates/post.html"
  writeFile' (outputFolder </> "posts" </> T.unpack postUrl) . T.unpack $ substitute template fullPostData
  convert fullPostData

-- | compile research/contents.md and output to research/contents.html
buildResearch, buildResearchE :: Action ()
buildResearch = cacheAction ("research" :: T.Text, "site/research/contents.md" :: FilePath) $ do
    researchContent <- readFile' "site/research/contents.md"
    researchData <- markdownToHTML . T.pack $ researchContent
    researchT <- compileTemplate' "site/templates/research.html"
    writeFile' (outputFolder </> "research/research.html") (T.unpack $ substitute researchT researchData)
buildResearchE = cacheAction ("research" :: T.Text, "site/research/contents-e.md" :: FilePath) $ do
    researchContent <- readFile' "site/research/contents-e.md"
    researchData <- markdownToHTML . T.pack $ researchContent
    researchT <- compileTemplate' "site/templates/research-e.html"
    writeFile' (outputFolder </> "research/research-e.html") (T.unpack $ substitute researchT researchData)


-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
    filepaths <- getDirectoryFiles "site/" ["images//*", "css//*", "research//*"]
    void $ forP filepaths $ \filepath ->
        copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

formatDate :: String -> String
formatDate humanDate = toIsoDate parsedTime
  where
    parsedTime =
      parseTimeOrError True defaultTimeLocale "%e %b %Y" humanDate :: UTCTime

-- rfc3339 :: Maybe String
-- rfc3339 = Just "%H:%M:%SZ"
toIsoDate :: UTCTime -> String
-- toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat rfc3339)
toIsoDate = formatShow iso8601Format

buildFeed :: [Post] -> Action ()
buildFeed posts' = do
  now <- liftIO getCurrentTime
  let atomData =
        AtomData
          { title = siteTitle siteMeta
          , domain = baseUrl siteMeta
          , author = siteAuthor siteMeta
          , posts = mkAtomPost <$> posts'
          , currentTime = toIsoDate now
          , atomUrl = "/atom.xml"
          }
  atomTempl <- compileTemplate' "site/templates/atom.xml"
  writeFile' (outputFolder </> "atom.xml") . T.unpack $ substitute atomTempl (toJSON atomData)
    where
      mkAtomPost :: Post -> Post
      mkAtomPost p = p { date = formatDate $ date p }

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  allPosts <- buildPosts
  allTags <- buildTagPages allPosts
  buildIndex allPosts allTags
  buildIndexE
  buildFeed allPosts
  copyStaticFiles
  -- added
  buildResearch
  buildResearchE

main :: IO ()
main = do
  let shOpts = shakeOptions { shakeVerbosity = Verbose, shakeLintInside = ["\\"],
                              shakeRebuild = [(RebuildNow, outputFolder </> "tags//*.html")]} -- tag lists needs rebuid everytime
  shakeArgsForward shOpts buildRules
