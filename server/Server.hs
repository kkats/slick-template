module Main where
--
-- https://remusao.github.io/posts/simple-http-server-haskell.html
--
import Network.Wai.Handler.Warp (run)
import Network.Wai.Application.Static

main :: IO ()
main = run 8080 (staticApp (defaultFileServerSettings "./docs"))
