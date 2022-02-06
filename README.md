# Source for dimitrije.website

To build just run `make`. To serve locally, run `make watch`.

## TeX CV

Install minimum TeX distribution with `brew install --cask basictex`, and then
run `make cv`. You will need additional packages from CTAN, most notably:

- `fontawesome`
- `sectsty`
- `enumitem`
- `titling`
- `wrapfig`

Install missing packages via `sudo tlmgr install <package>`. You will most
likely have to update `tlmgr` before that, and possibly change to a more
up-to-date CTAN mirror with:

```bash
$ sudo tlmgr option repository "https://packages.oth-regensburg.de/ctan/systems/texlive/tlnet"
$ sudo tlmgr update --self
```

Also, you will need the following line in the TeX file:

```
\defaultfontfeatures{Path = /usr/local/texlive/2021basic/texmf-dist/fonts/opentype/public/fontawesome/}
```

Make sure that the path is correct.

## Syntax Highlighting with Pygments

Take a look at https://gist.github.com/scturtle/9236b7208aab9985a3af0d319bdb4041#file-pygments-hs. Also disable Pandoc's syntax higlighting in writerOptions.

```haskell
pygmentizePandocCompiler :: Compiler (Item String)
pygmentizePandocCompiler = pandocCompilerWithTransformM defaultHakyllReaderOptions html5WriterOptions pygmentize

html5WriterOptions :: WriterOptions
html5WriterOptions = defaultHakyllWriterOptions { writerHighlightStyle = Nothing }

pygmentize :: Pandoc -> Compiler Pandoc
pygmentize (Pandoc meta bs) = Pandoc meta <$> mapM highlight bs

highlight :: Block -> Compiler Block
highlight (CodeBlock (_, options, _) code) =
  RawBlock "html" <$> unsafeCompiler (pack <$> pygments (unpack code) (unpack <$> options))
highlight x = return x

pygments :: String -> [String] -> IO String
pygments code options =
  case options of
    (lang : _) -> readProcess "pygmentize" ["-l", toLower <$> lang, "-f", "html"] code
    _ -> return $ "<div class =\"highlight\"><pre>" ++ code ++ "</pre></div>"
```
