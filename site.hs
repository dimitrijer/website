--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.List (intercalate)
import qualified Data.Map as M
import Data.Monoid (mappend)
import qualified Data.Text as DT
import Data.Time
import Hakyll
import Skylighting.Loader (loadSyntaxesFromDir)
import Skylighting.Syntax (defaultSyntaxMap)
import qualified Skylighting.Types as ST
import System.Process (readProcess)
import Text.Pandoc
import Text.Pandoc.Highlighting
import Text.Pandoc.Walk (walk)

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

colorIvory = ST.RGB 255 255 245

colorArtichoke = ST.RGB 126 132 107

colorKobe = ST.RGB 144 32 0

colorPersianBlue = ST.RGB 0 51 204

colorBlack = ST.RGB 0 0 0

pandocHighlightStyle :: Style
pandocHighlightStyle =
  kate
    { ST.defaultColor = Just colorBlack,
      ST.backgroundColor = Just colorIvory,
      ST.lineNumberBackgroundColor = Just colorIvory,
      ST.tokenStyles = customTokenStyles
    }
  where
    customTokenStyles =
      M.fromList
        [ (ST.CommentTok, ST.defStyle {ST.tokenColor = Just colorArtichoke}),
          (ST.DataTypeTok, ST.defStyle {ST.tokenColor = Just colorPersianBlue}),
          (ST.StringTok, ST.defStyle {ST.tokenColor = Just colorKobe})
        ]
        `mappend` ST.tokenStyles kate

colorCream = ST.RGB 231 226 211

colorSurfaceDark = ST.RGB 42 35 27

colorMuted = ST.RGB 168 173 149

pandocHighlightStyleDark :: Style
pandocHighlightStyleDark =
  breezeDark
    { ST.defaultColor = Just colorCream,
      ST.backgroundColor = Just colorSurfaceDark,
      ST.lineNumberColor = Just colorMuted,
      ST.lineNumberBackgroundColor = Just colorSurfaceDark
    }

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

beautifyHTML :: Item String -> Compiler (Item String)
beautifyHTML item = do
  output <- recompilingUnsafeCompiler (readProcess "prettier" ["--no-config", "--print-width", "120", "--parser", "html"] (itemBody item))
  return $ fmap (const output) item

-- Duplicate each footnote inline as a sidenote, while leaving Pandoc's native
-- end-of-document footnotes untouched. CSS shows one or the other depending on
-- whether there is room in the margin.
sidenoteTransform :: Pandoc -> Pandoc
sidenoteTransform = walk toSidenote
  where
    toSidenote :: Inline -> Inline
    toSidenote (Note blocks) =
      Span
        ("", [], [])
        [ Note blocks,
          Span ("", ["sidenote"], []) (concatBlockInlines blocks)
        ]
    toSidenote inline = inline

    concatBlockInlines :: [Block] -> [Inline]
    concatBlockInlines = intercalate [Space] . map blockInlines

    blockInlines :: Block -> [Inline]
    blockInlines (Plain ils) = ils
    blockInlines (Para ils) = ils
    blockInlines (LineBlock lss) = intercalate [LineBreak] lss
    blockInlines (Header _ _ ils) = ils
    blockInlines (CodeBlock _ t) = [Code ("", [], []) t]
    blockInlines (RawBlock _ t) = [RawInline "html" t]
    blockInlines (BlockQuote bs) = concatBlockInlines bs
    blockInlines (OrderedList _ items) = concatBlockInlines (concat items)
    blockInlines (BulletList items) = concatBlockInlines (concat items)
    blockInlines (DefinitionList items) = concatBlockInlines (concatMap (\(termInlines, defs) -> Para termInlines : concat defs) items)
    blockInlines (Div _ bs) = concatBlockInlines bs
    blockInlines _ = []

-- Wrap the first letter of a piece's opening paragraph so it can be styled as
-- a drop cap.
leadTransform :: Pandoc -> Pandoc
leadTransform (Pandoc meta blocks) = Pandoc meta (go blocks)
  where
    go (Para ils : rest) = Div ("", ["lead"], []) [Para (dropCapFirstLetter ils)] : rest
    go (block : rest) = block : go rest
    go [] = []

    dropCapFirstLetter :: [Inline] -> [Inline]
    dropCapFirstLetter (Str word : rest) =
      case DT.uncons word of
        Nothing -> Str word : rest
        Just (initial, remainder) ->
          Span ("", ["dropcap"], []) [Str (DT.singleton initial)]
            : [Str remainder | not (DT.null remainder)]
            ++ rest
    dropCapFirstLetter ils = ils

runHakyll :: ST.SyntaxMap -> IO ()
runHakyll sm =
  hakyllWith config $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "files/*" $ do
      route idRoute
      compile copyFileCompiler

    match "fonts/*" $ do
      route idRoute
      compile copyFileCompiler

    create ["css/syntax.css"] $ do
      route idRoute
      compile $ do
        makeItem $ compressCss . styleToCss $ pandocHighlightStyle

    create ["css/syntax-dark.css"] $ do
      route idRoute
      compile $ do
        makeItem $ compressCss . styleToCss $ pandocHighlightStyleDark

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
          >>= beautifyHTML

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
          >>= beautifyHTML

    match "pages/cv.md" $ do
      route $ constRoute "cv.html"
      compile $ do
        csl <- load $ fromFilePath "csl/ieee-with-url.csl"
        bib <- load $ fromFilePath "bib/refs.bib"
        getResourceBody
          >>= readPandocBiblio defaultHakyllReaderOptions csl bib
          >>= return . fmap sidenoteTransform
          >>= return . writePandocWith html5WriterOptions
          >>= loadAndApplyTemplate "templates/default.html" (singlePageCtx `mappend` constField "htmltitle" "Curriculum Vitae")
          >>= relativizeUrls
          >>= beautifyHTML

    match "pages/contact.md" $ do
      route $ constRoute "contact.html"
      compile $
        pandocCompilerWithTransform defaultHakyllReaderOptions html5WriterOptions sidenoteTransform
          >>= loadAndApplyTemplate "templates/default.html" (singlePageCtx `mappend` constField "htmltitle" "Contact")
          >>= relativizeUrls
          >>= beautifyHTML

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
          >>= return . fmap sidenoteTransform
          >>= return . writePandocWith html5WriterOptions
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls
          >>= beautifyHTML

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
    customPandocCompiler = pandocCompilerWithTransform
      defaultHakyllReaderOptions
      html5WriterOptions
      { writerSyntaxMap = defaultSyntaxMap `mappend` sm
      , writerCiteMethod = Citeproc
      }
      (sidenoteTransform . leadTransform)

main :: IO ()
main = loadSyntaxesFromDir "syntax" >>= either fail runHakyll

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  constField "root" root
    `mappend` dateField "date" "%Y-%m-%d"
    `mappend` dateField "dateLong" "%-d %B %Y"
    `mappend` defaultContext

singlePageCtx :: Context String
singlePageCtx =
  constField "root" root
    `mappend` modificationTimeField "updated" "%Y-%m-%d"
    `mappend` defaultContext
