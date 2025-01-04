# A fork of [slick-template](https://github.com/ChrisPenner/slick-template)

I have built my static site with [slick](https://github.com/ChrisPenner/slick) by modifying the template.
The blog posts in site/posts need to have a filename starting from `YYMMDD-`.  If the 7th character is `M`
(e.g. '20250102M-test.md'), a MathJax capability is activated such that you can use Latex syntax for
math symbols.

+ Removed [stack](https://docs.haskellstack.org/en/stable/) portability
+ Added tag pages following [ひげメモ](https://matsubara0507.github.io/posts/2021-06-13-my-site-use-slick.html).
+ Twitter → Bluesky
+ MathJax capability for blog posts
+ Added test server capability after [A simple HTTP server in Haskell](https://docs.haskellstack.org/en/stable/)
