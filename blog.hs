{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Prelude hiding ( id )

import Control.Arrow ( arr, (>>>) )
import Data.List ( intercalate, sortBy )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( mempty, mconcat )
import Data.Ord ( comparing )
import Data.Time
import System.Locale
import Text.Regex.TDFA hiding ( match )

import Hakyll

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  match "templates/*" $ compile templateCompiler

  match (list ["about.md"]) $ do
    route $ setExtension "html"
    compile $ pageCompiler
      >>> applyTemplateCompiler "templates/about.html"
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

  match "index.html" $ route idRoute
  create "index.html" $ constA mempty
    >>> arr (setField "title" "Tristan.Blog")
    >>> setFieldPageList (take 5 . reverse . postOrder) "templates/postitem.html" "posts" "posts/*/*.md"
    >>> applyTemplateCompiler "templates/index.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler

  match "archive.html" $ route idRoute
  create "archive.html" $ constA mempty
    >>> arr (setField "title" "Archive")
    >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud')
    >>> requireAllA "posts/*/*.md" (addPostList "templates/post-summary.html")
    >>> applyTemplateCompiler "templates/archive.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler

  -- Note that this step copies the blog post body into the $content$
  -- key so that it can be accessed later from index-like pages.
  -- Without this, only $body$ is available, but that includes the
  -- surrounding chrome (like the nav menu) and we don't want to
  -- duplicate that for every post in a list.
  match "posts/*/*.md" $ do
    route $ setExtension "html"
    compile $ pageCompiler
      >>> arr (customRenderDateField "date" "%B %e, %Y" "unknown")
      >>> renderTagsField "prettytags" tagIdentifier
      >>> arr (copyBodyToField "contents")
      >>> arr (copyBodyToField "description")
      >>> applyTemplateCompiler "templates/post.html"
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

  -- Build tags
  create "tags" $ requireAll "posts/*/*.md" (\_ ps -> readTags ps :: Tags String)
  match "tags/*" $ route $ setExtension ".html"
  metaCompile $ require_ "tags"
    >>> arr tagsMap
    >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

  match "rss.xml" $ route idRoute
  create "rss.xml" $ requireAll_ "posts/*/*.md"
    >>> renderRss feedConfiguration

  return ()

renderTagCloud' :: Compiler (Tags String) String
renderTagCloud' = renderTagCloud tagIdentifier 75 400

tagIdentifier :: String -> Identifier a
tagIdentifier = fromCapture "tags/*"

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Identifier Template -> Compiler (Page String, [Page String]) (Page String)
addPostList tpl = setFieldA "posts" $ arr (reverse . postOrder)
  >>> require tpl (\p t -> map (applyTemplate t) p)
  >>> arr mconcat
  >>> arr pageBody

makeTagList :: String
               -> [Page String]
               -> Compiler () (Page String)
makeTagList tag posts =
  constA (mempty, posts)
    >>> addPostList "templates/post-summary.html"
    >>> requireA "tags" (setFieldA "taglist" renderTagCloud')
    >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
    >>> applyTemplateCompiler "templates/index.html"
    >>> applyTemplateCompiler "templates/default.html"


-- | This is just like renderDateField, except the format of the
-- file path is expected to be: year/month-day-title
customRenderDateField :: String -> String -> String -> Page a -> Page a
customRenderDateField =
  customRenderDateFieldWith defaultTimeLocale

customRenderDateFieldWith :: TimeLocale  -- ^ Output time locale
                             -> String      -- ^ Destination key
                             -> String      -- ^ Format to use on the date
                             -> String      -- ^ Default value
                             -> Page a      -- ^ Target page
                             -> Page a      -- ^ Resulting page
customRenderDateFieldWith locale key format defaultValue =
  renderField "path" key renderDate'
  where
    nameRx :: String
    nameRx = ".*/([[:digit:]]+)/([[:digit:]]+)-([[:digit:]]+)-.*"

    renderDate' filePath = fromMaybe defaultValue $ do
      let comps :: [String]
          MR { mrSubList = comps } = filePath =~ nameRx
          dateString = intercalate "-" comps
      time <- parseTime defaultTimeLocale "%Y-%m-%d" dateString :: Maybe UTCTime
      return $ formatTime locale format time

-- | A basic RSS feed configuration
feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle       = "Tristan.RSS"
    , feedDescription = "RSS feed for nochair.net"
    , feedAuthorName  = "Tristan Ravitch"
    , feedRoot        = "http://nochair.net"
    }

-- | The default 'chronological' function assumes yyyy-mm-dd-title.md
-- filenames.  This simpler version works with my modified
-- yyyy/mm-dd-title.md format.
postOrder :: [Page a] -> [Page a]
postOrder = sortBy $ comparing $ getField "path"
