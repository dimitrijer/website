--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M
import Data.Monoid (mappend)
import Data.Time
import Hakyll
import Skylighting.Loader (loadSyntaxesFromDir)
import Skylighting.Syntax (defaultSyntaxMap)
import qualified Skylighting.Types as T
import Text.Pandoc
import Text.Pandoc.Highlighting

--------------------------------------------------------------------------------
root :: String
root = "https://dimitrije.website"

config :: Configuration
config =
  defaultConfiguration
    { deployCommand = "rsync -av _site/* dimitrije.website:/srv/http",
      previewHost = "127.0.0.1",
      previewPort = 8082
    }

colorIvory = T.RGB 255 255 245

colorArtichoke = T.RGB 126 132 107

colorKobe = T.RGB 144 32 0

colorPersianBlue = T.RGB 0 51 204

colorBlack = T.RGB 0 0 0

pandocHighlightStyle :: Style
pandocHighlightStyle =
  kate
    { T.defaultColor = Just colorBlack,
      T.backgroundColor = Just colorIvory,
      T.lineNumberBackgroundColor = Just colorIvory,
      T.tokenStyles = customTokenStyles
    }
  where
    customTokenStyles =
      M.fromList
        [ (T.CommentTok, T.defStyle {T.tokenColor = Just colorArtichoke}),
          (T.DataTypeTok, T.defStyle {T.tokenColor = Just colorPersianBlue}),
          (T.StringTok, T.defStyle {T.tokenColor = Just colorKobe})
        ]
        `mappend` T.tokenStyles kate

html5WriterOptions :: WriterOptions
html5WriterOptions =
  defaultHakyllWriterOptions
    { writerSectionDivs = True,
      writerHTMLMathMethod = MathJax "",
      writerTopLevelDivision = TopLevelSection,
      writerCiteMethod = Citeproc,
      writerHighlightStyle = Just pandocHighlightStyle
    }

rssFeedConfiguration :: FeedConfiguration
rssFeedConfiguration =
  FeedConfiguration
    { feedTitle = "Dimitrije's Website",
      feedDescription = "Feed of fresh posts from Dimitrije's Website",
      feedAuthorName = "Dimitrije Radojević",
      feedAuthorEmail = "me@dimitrije.website",
      feedRoot = "https://dimitrije.website"
    }

runHakyll :: T.SyntaxMap -> IO ()
runHakyll sm =
  hakyllWith config $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "files/*" $ do
      route idRoute
      compile copyFileCompiler

    create ["css/syntax.css"] $ do
      route idRoute
      compile $ do
        makeItem $ compressCss . styleToCss $ pandocHighlightStyle

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match "bib/*" $ compile biblioCompiler
    match "csl/*" $ compile cslCompiler

    match "posts/*" $ do
      route $ setExtension "html"
      compile $
        customPandocCompiler
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        mostRecentPostDate <- getItemModificationTime (itemIdentifier $ head posts)

        let archiveCtx =
              constField "root" root
                `mappend` listField "posts" postCtx (return posts)
                `mappend` boolField "somePosts" (return $ not (null posts))
                `mappend` constField "htmltitle" "Archives"
                `mappend` constField "updated" (formatTime defaultTimeLocale "%Y-%m-%d" mostRecentPostDate)
                `mappend` constField "path" "templates/archive.html"
                `mappend` defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx

    match "pages/cv.md" $ do
      route $ constRoute "cv.html"
      compile $ do
        csl <- load $ fromFilePath "csl/ieee-with-url.csl"
        bib <- load $ fromFilePath "bib/refs.bib"
        getResourceBody
          >>= readPandocBiblio defaultHakyllReaderOptions csl bib
          >>= return . writePandocWith html5WriterOptions
          >>= loadAndApplyTemplate "templates/default.html" (singlePageCtx `mappend` constField "htmltitle" "Curriculum Vitae")
          >>= relativizeUrls

    match "pages/contact.md" $ do
      route $ constRoute "contact.html"
      compile $
        pandocCompilerWith defaultHakyllReaderOptions html5WriterOptions
          >>= loadAndApplyTemplate "templates/default.html" (singlePageCtx `mappend` constField "htmltitle" "Contact")
          >>= relativizeUrls

    match "pages/main.md" $ do
      route $ constRoute "index.html"
      compile $ do
        posts <- fmap (take 5) . recentFirst =<< loadAll "posts/*"

        let indexCtx =
              listField "posts" postCtx (return posts)
                `mappend` boolField "somePosts" (return $ not (null posts))
                `mappend` constField "root" root
                `mappend` modificationTimeField "updated" "%Y-%m-%d"
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
        singlePages <- loadAll (fromList ["pages/contact.md", "pages/cv.md"])

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

    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <-
          fmap (take 10) . recentFirst
            =<< loadAllSnapshots "posts/*" "content"
        renderRss rssFeedConfiguration feedCtx posts

    match "templates/*" $ compile templateCompiler
  where
    customPandocCompiler = pandocCompilerWith defaultHakyllReaderOptions html5WriterOptions {writerSyntaxMap = defaultSyntaxMap `mappend` sm}

main :: IO ()
main = loadSyntaxesFromDir "syntax" >>= either fail runHakyll

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
