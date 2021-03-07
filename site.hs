--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Data.Time
import Hakyll
import Text.Pandoc

--------------------------------------------------------------------------------
root :: String
root = "https://dimitrije.website"

html5WriterOptions :: WriterOptions
html5WriterOptions =
  defaultHakyllWriterOptions
    { writerSectionDivs = True,
      writerHTMLMathMethod = MathJax "",
      writerTopLevelDivision = TopLevelSection,
      writerCiteMethod = Citeproc
    }

config :: Configuration
config =
  defaultConfiguration
    { deployCommand = "rsync -av _site/* dimitrije.website:/srv/http",
      previewHost = "0.0.0.0",
      previewPort = 8080
    }

main :: IO ()
main = hakyllWith config $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "bib/*" $ compile biblioCompiler
  match "csl/*" $ compile cslCompiler

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      mostRecentPostDate <- getItemModificationTime (itemIdentifier $ head posts)

      let archiveCtx =
            constField "root" root
              `mappend` constField "updated" (formatTime defaultTimeLocale "%Y-%m-%d" mostRecentPostDate)
              `mappend` listField "posts" postCtx (return posts)
              `mappend` boolField "somePosts" (return $ (length posts) > 0)
              `mappend` constField "htmltitle" "Archives"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx

  match "cv.md" $ do
    route $ setExtension "html"
    compile $ do
      csl <- load $ fromFilePath "csl/ieee-with-url.csl"
      bib <- load $ fromFilePath "bib/refs.bib"
      getResourceBody
        >>= readPandocBiblio defaultHakyllReaderOptions csl bib
        >>= return . writePandocWith html5WriterOptions
        >>= loadAndApplyTemplate "templates/default.html" singlePageCtx
        >>= relativizeUrls

  match "contact.md" $ do
    route $ setExtension "html"
    compile $
      pandocCompilerWith defaultHakyllReaderOptions html5WriterOptions
        >>= loadAndApplyTemplate "templates/default.html" singlePageCtx
        >>= relativizeUrls

  match "main.md" $ do
    route $ customRoute $ (\_ -> "index.html")
    compile $ do
      posts <- fmap (take 3) . recentFirst =<< loadAll "posts/*"

      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` boolField "somePosts" (return $ (length posts) > 0)
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= readPandocWith defaultHakyllReaderOptions
        >>= return . writePandocWith html5WriterOptions
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      singlePages <- loadAll (fromList ["contact.md", "cv.md"])

      timeZone <- unsafeCompiler Data.Time.getCurrentTimeZone
      currentTime <- unsafeCompiler Data.Time.getCurrentTime

      mostRecentPostDate <- getItemModificationTime (itemIdentifier $ head posts)

      let localTime = utcToZonedTime timeZone currentTime
      let sitemapCtx =
            constField "root" root
              `mappend` constField "updated" (formatTime defaultTimeLocale "%Y-%m-%d" localTime)
              `mappend` constField "mostRecentPostDate" (formatTime defaultTimeLocale "%Y-%m-%d" mostRecentPostDate)
              `mappend` listField "posts" postCtx (return posts)
              `mappend` listField "singlePages" singlePageCtx (return singlePages)
      makeItem ""
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  constField "root" root
    `mappend` dateField "date" "%Y-%m-%d"
    `mappend` defaultContext

singlePageCtx :: Context String
singlePageCtx =
  constField "root" root
    `mappend` modificationTimeField "updated" "%Y-%m-%d"
    `mappend` defaultContext
