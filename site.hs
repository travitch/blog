{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Data.Monoid ( (<>), mconcat )
import Hakyll

-- How many posts to show in limited-space contexts
defaultRecentCount :: Int
defaultRecentCount = 10

-- Pattern to pick up all posts
postPattern :: Pattern
postPattern = "posts/*/*.md"

-- The key under which we store post contents
contentSnapshot :: String
contentSnapshot = "contents"

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- A static about page
    match (fromList ["about.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags postPattern $ fromCapture "tags/*.html"

    -- Create a "tags/<tagname>.html" for each tag.  Each one of these
    -- pages is a list of all posts with the given tag.
    tagsRules tags $ \tag pattern -> do
      let title = "Posts tagged " ++ tag
      route idRoute
      compile $ do
        posts <- postList tags pattern recentFirst
        let ctx = constField "title" title <>
                   constField "posts" posts <> defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/tagged.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    -- This rule generates each individual post page
    match postPattern $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot contentSnapshot
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    -- The overall archives page has a list of all old posts, in
    -- order.  It also has a tag cloud at the top, with links to a
    -- list of posts with each tag.
    create ["archive.html"] $ do
        route idRoute
        compile $ do
          posts <- constField "posts" <$> postList tags postPattern recentFirst
          let archiveCtx = mconcat [ posts
                                   , tagCloudField "tagcloud" 50 300 tags
                                   , constField "title" "Archives"
                                   , defaultContext
                                   ]

          makeItem ""
            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
            >>= relativizeUrls

    -- The index page has a list of the most recent posts.
    create ["index.html"] $ do
        route idRoute
        compile $ do
          posts <- constField "posts" <$> postListC tags takeRecent
          let indexCtx = mconcat [ posts, constField "title" "Blog", defaultContext ]

          makeItem ""
            >>= loadAndApplyTemplate "templates/index.html" indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

    -- Build a simple RSS feed with the most recent posts
    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        allPosts <- loadAllSnapshots postPattern contentSnapshot
        posts <- takeRecent allPosts
        let feedCtx = bodyField "description" <> defaultContext
        renderRss feedConfig feedCtx posts

    match "templates/*" $ compile templateCompiler

-- Helpers

takeRecent :: [Item a] -> Compiler [Item a]
takeRecent = fmap (take defaultRecentCount) . recentFirst

--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = mconcat [ dateField "published" "%B %e, %Y"
                       , tagsField "prettytags" tags
                       , defaultContext
                       ]


--------------------------------------------------------------------------------
postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList tags pattern sortFilter = do
  posts   <- sortFilter =<< loadAllSnapshots pattern contentSnapshot
  itemTpl <- loadBody "templates/post-summary.html"
  applyTemplateList itemTpl (postCtx tags) posts

postListC :: Tags -> ([Item String] -> Compiler [Item String]) -> Compiler String
postListC tags sortFilter = do
  posts <- sortFilter =<< loadAllSnapshots postPattern contentSnapshot
  -- posts <- sortFilter =<< loadAll postPattern
  itemTpl <- loadBody "templates/postitem.html"
  applyTemplateList itemTpl (postCtx tags) posts

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration { feedTitle = "Tristan's Recent Posts"
                               , feedDescription = "Programming, Haskell, and Linux"
                               , feedAuthorName = "Tristan Ravitch"
                               , feedAuthorEmail = "tristan@nochair.net"
                               , feedRoot = "http://nochair.net"
                               }

