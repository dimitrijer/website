--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Control.Monad (liftM)
import           Hakyll
import           Hakyll.Web.Pandoc
import           Text.Pandoc


--------------------------------------------------------------------------------
root :: String
root = "https://dimitrije.website"

html5WriterOptions :: WriterOptions
html5WriterOptions = defaultHakyllWriterOptions
    { writerSectionDivs = True
    , writerHTMLMathMethod = MathJax ""
    , writerTopLevelDivision = TopLevelSection
    , writerCiteMethod = Citeproc
    }

pandocBiblioCompilerWith :: String -> String -> WriterOptions -> Compiler (Item String)
pandocBiblioCompilerWith cslFileName bibFileName writerOpts = do
    csl <- load $ fromFilePath cslFileName
    bib <- load $ fromFilePath bibFileName
    liftM (writePandocWith writerOpts)
        (getResourceBody >>= readPandocBiblio defaultHakyllReaderOptions csl bib)

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "bib/*" $ compile biblioCompiler
    match "csl/*" $ compile cslCompiler

    -- match (fromList ["main.md"]) $ do
    --     route   $ setExtension "html"
    --     compile $ pandocCompiler
    --         >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --         >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "htmltitle" "Archives"        `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx

    match "contact.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions html5WriterOptions
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "main.md" $ do
        route   $ customRoute $ (\x -> "index.html")
        compile $ do
            csl <- load $ fromFilePath "csl/ieee-with-url.csl"
            bib <- load $ fromFilePath "bib/refs.bib"
            posts <- fmap (take 3) . recentFirst =<< loadAll "posts/*"

            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= readPandocBiblio defaultHakyllReaderOptions csl bib
                >>= return . writePandocWith html5WriterOptions
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["sitemap.xml"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                singlePages <- loadAll (fromList ["contact.md", "main.md"])
                let pages = posts <> singlePages
                    sitemapCtx =
                        constField "root" root <> -- here
                        listField "pages" postCtx (return pages)
                makeItem ""
                    >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    constField "root" root `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

