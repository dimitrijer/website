# CLAUDE.md - dimitrije.website

This file provides comprehensive guidance for working with the dimitrije.website codebase. This is a personal website and technical blog built with Hakyll (Haskell static site generator).

## Project Overview

**Website**: https://dimitrije.website
**Author**: Dimitrije Radojević
**Primary Language**: Haskell
**Framework**: Hakyll (static site generator)
**Build System**: Nix + Cabal + Make
**Deployment**: rsync to dimitrije.website:/srv/http

### Key Features
- Technical blog with custom syntax highlighting (Bash, Nix, OCaml, RouterOS, Terraform)
- PDF CV generation using Pandoc + XeLaTeX
- RSS feed generation
- Custom typography with web fonts (Charter, Fira Mono)
- Responsive design focused on readability
- MathJax support for LaTeX equations

### Licenses
- **Source code**: LGPL-3.0
- **Content**: CC BY 4.0

## Technology Stack

### Core Technologies
- **Haskell**: Site generation logic (site.hs - 224 lines)
- **Hakyll 4.13+**: Static site generator framework
- **Pandoc 2.7+**: Markdown → HTML conversion, document processing
- **Skylighting**: Syntax highlighting with custom language definitions
- **Nix**: Reproducible build environment and dependency management
- **Cabal 3.4+**: Haskell package management

### Additional Tools
- **Prettier**: HTML formatting (replacing js-beautify)
- **XeLaTeX**: PDF CV generation
- **ripgrep**: Fast text search during development
- **ImageMagick**: Image processing utilities
- **Ormolu**: Haskell code formatter (in dev environment)
- **nixpkgs-fmt**: Nix file formatter

### Frontend
- HTML5 with semantic markup
- CSS3 with custom color palette and typography
- WOFF2 web fonts (Charter serif, Fira Mono, Droid Sans Mono)
- MathJax for mathematical notation

## Quick Start Guide

### Prerequisites
- **Nix package manager** (required)
- Git

### Initial Setup

```bash
# Clone repository (if needed)
git clone <repo-url>
cd website

# Enter Nix development environment
nix-shell

# Build the site generator
cabal build

# Serve locally with hot reload
make watch
# Site will be available at http://127.0.0.1:8082
```

### Common Commands

```bash
# Development
make watch        # Start dev server with auto-rebuild (port 8082)
make build        # Production build to _site/
make rebuild      # Clean rebuild from scratch
make clean        # Remove generated files

# Content
make cv           # Generate PDF CV from pages/cv.md

# Deployment
make deploy       # Rsync to production server
```

## Project Structure

```
/home/user/website/
├── site.hs                      # Main Hakyll site generator (224 lines)
├── dimitrije-website.cabal      # Haskell package definition
├── Makefile                     # Build automation
├── README.md                    # Basic project info
├── shell.nix                    # Nix development environment
├── default.nix                  # Nix build convenience wrapper
├── hie.yml                      # Haskell IDE Engine config
├── .ghci                        # GHCi REPL configuration
│
├── nix/                         # Nix configuration
│   ├── default.nix              # Nixpkgs import with overlays
│   ├── site.nix                 # Hakyll site derivation
│   ├── overlays.nix             # GHC with Haskell packages
│   ├── sources.json             # Pinned dependencies (Niv)
│   └── sources.nix              # Dependency fetching logic
│
├── pages/                       # Static pages (Markdown)
│   ├── main.md                  # Homepage → index.html
│   ├── cv.md                    # CV → cv.html (with citations)
│   └── contact.md               # Contact page → contact.html
│
├── posts/                       # Blog posts (8 posts)
│   └── YYYY-MM-DD-slug.md       # Blog posts with YAML frontmatter
│
├── templates/                   # Hakyll templates
│   ├── default.html             # Main layout (header + footer)
│   ├── post.html                # Blog post wrapper
│   ├── archive.html             # Post archive listing
│   ├── header.html              # Navigation header (partial)
│   ├── footer.html              # Footer with metadata (partial)
│   ├── post-list.html           # Post list component
│   ├── post-recent-list.html    # Recent posts component
│   ├── sitemap.xml              # XML sitemap template
│   └── cv-template.tex          # LaTeX CV template
│
├── css/                         # Stylesheets
│   ├── style.css                # Main styles (6KB)
│   └── fonts.css                # Font-face declarations
│
├── fonts/                       # Web fonts (WOFF2, 268 KB)
│   ├── charter_*.woff2          # Charter serif (regular, italic, bold, SC)
│   ├── droid_sans_mono.woff2    # Monospace font
│   └── firamono_*.woff2         # Fira Mono (regular, bold)
│
├── images/                      # Website assets (13 MB)
│   ├── *.png, *.jpg, *.webp     # Blog images, icons, favicons
│   ├── site.webmanifest         # PWA manifest
│   └── browserconfig.xml        # Windows tile config
│
├── files/                       # Downloadable files (13 MB)
│   ├── CV_Dimitrije_Radojevic.pdf  # Generated PDF CV
│   └── *.stl, *.FCStd, *.pdf    # CAD models, documents
│
├── syntax/                      # Custom syntax highlighting (206 KB)
│   ├── bash.xml                 # Bash syntax (1,901 lines)
│   ├── nix.xml                  # Nix language
│   ├── ocaml.xml                # OCaml syntax
│   ├── routeros.xml             # MikroTik RouterOS
│   ├── terraform.xml            # Terraform IaC
│   └── language.dtd             # Syntax definition DTD
│
├── bib/                         # Bibliography
│   └── refs.bib                 # BibTeX references for CV
│
├── csl/                         # Citation styles
│   └── ieee-with-url.csl        # IEEE citation format with URLs
│
├── _site/                       # Generated output (gitignored)
└── _cache/                      # Hakyll cache (gitignored)
```

## Development Workflow

### Nix Environment

All development happens inside `nix-shell`:

```bash
nix-shell
# Provides: ghc, cabal, haskell-language-server, ormolu,
#           nixpkgs-fmt, prettier, ripgrep, imagemagick,
#           neovim, latex/xetex
```

**Shell aliases available**:
- `ll` → `ls -alh --color=auto`
- `ls` → `ls -ah --color=auto`
- `vim` → `nvim`

### Build Process

1. **site.hs** is compiled to executable `site`
2. `site` command runs Hakyll compilation pipeline
3. Markdown files → Pandoc → HTML
4. Templates applied recursively
5. HTML formatted with Prettier
6. Static files copied to `_site/`

### Watch Mode Development

```bash
make watch
# - Starts server on http://127.0.0.1:8082
# - Watches for file changes
# - Auto-rebuilds on save
# - Live preview in browser
```

**What triggers rebuilds**:
- Markdown content (posts/*, pages/*)
- Templates (templates/*)
- CSS files (css/*)
- site.hs changes (requires restart)

### Adding New Content

#### Blog Posts

1. Create file: `posts/YYYY-MM-DD-title-slug.md`
2. Add YAML frontmatter:
   ```yaml
   ---
   title: Post Title
   subtitle: Optional subtitle
   date: YYYY-MM-DD
   author: Optional author override
   og_description: Social media description
   cover_image: /images/cover.jpg
   ---
   ```
3. Write content in Markdown
4. File automatically appears in archive and RSS feed
5. 5 most recent posts show on homepage

**Post context variables** (site.hs:214-218):
- `$title$`, `$subtitle$`, `$author$`
- `$date$` (formatted as YYYY-MM-DD)
- `$root$` (https://dimitrije.website)
- `$body$` (rendered HTML content)
- `$og_description$`, `$cover_image$`

#### Static Pages

1. Create file: `pages/page-name.md`
2. Add route in site.hs:
   ```haskell
   match "pages/page-name.md" $ do
     route $ constRoute "page-name.html"
     compile $ pandocCompilerWith defaultHakyllReaderOptions html5WriterOptions
       >>= loadAndApplyTemplate "templates/default.html" singlePageCtx
       >>= relativizeUrls
       >>= beautifyHTML
   ```
3. Update navigation in templates/header.html if needed

#### Custom Syntax Highlighting

To add new language support:
1. Create `syntax/language-name.xml` (Skylighting XML format)
2. Syntax files auto-loaded from `syntax/` directory (site.hs:211)
3. Reference: https://docs.kde.org/stable5/en/kate/katepart/highlight.html

## Build & Deployment

### Production Build

```bash
make all
# Equivalent to: make clean && make cv && make build
```

**Build outputs**:
- `_site/index.html` - Homepage with 5 recent posts
- `_site/cv.html` - CV page with citations
- `_site/contact.html` - Contact page
- `_site/archive.html` - All posts archive
- `_site/rss.xml` - RSS feed (10 recent posts)
- `_site/sitemap.xml` - XML sitemap
- `_site/css/syntax.css` - Generated syntax highlighting CSS
- `_site/posts/*.html` - Individual post pages
- Static assets: images, fonts, files

### PDF CV Generation

```bash
make cv
```

**Process** (Makefile:32-45):
1. Substitute TEXLIVE_PATH in LaTeX template
2. Run Pandoc with:
   - Markdown input: `pages/cv.md`
   - Template: `templates/cv-template.tex`
   - Bibliography: `bib/refs.bib`
   - CSL style: `csl/ieee-with-url.csl` (IEEE with URLs)
   - Engine: XeLaTeX
3. Output: `files/CV_Dimitrije_Radojevic.pdf`

**Features**:
- FontAwesome icons
- IEEE citation style
- Professional typography
- Automatic bibliography

### Deployment

```bash
make deploy
```

**Deployment configuration** (site.hs:22):
- Command: `rsync -av _site/* dimitrije.website:/srv/http`
- Archive mode with verbose output
- Syncs entire `_site/` to production server

**Manual deployment** (if needed):
```bash
rsync -av _site/* dimitrije.website:/srv/http
```

## Code Organization Patterns

### Hakyll Architecture (site.hs)

**Pattern**: Monadic compiler pipeline with template application

```haskell
compile $
  customPandocCompiler              -- Parse Markdown
    >>= loadAndApplyTemplate "..." -- Apply inner template
    >>= saveSnapshot "content"     -- Save for RSS
    >>= loadAndApplyTemplate "..." -- Apply outer template
    >>= relativizeUrls             -- Make URLs relative
    >>= beautifyHTML               -- Format with Prettier
```

### Key Functions

**beautifyHTML** (site.hs:74-77):
- Formats HTML using Prettier
- Config: `--no-config --print-width 120 --parser html`
- Applied to all HTML output

**customPandocCompiler** (site.hs:208):
- Merges default syntax map with custom syntax from `syntax/`
- Uses `html5WriterOptions` for consistent output

**Contexts** (site.hs:214-224):
- `postCtx`: root URL, date field, defaults
- `singlePageCtx`: root URL, modification time, defaults
- Composable with `mappend` (monoid operation)

### Configuration (site.hs:16-72)

**Site config** (lines 16-25):
- Root URL: https://dimitrije.website
- Deploy command: rsync to production
- Preview: 127.0.0.1:8082

**Color palette** (lines 27-34):
- Ivory (#fffff5) - Background
- Black (#000000) - Text
- Kobe Red (#902000) - Emphasis
- Persian Blue (#0033cc) - Links
- Artichoke (#7e846b) - Comments

**Pandoc options** (lines 54-62):
- Section divs enabled
- MathJax for equations
- Citeproc for citations
- Custom syntax highlighting theme

**RSS config** (lines 64-72):
- Feed title, description, author
- Last 10 posts with full content

### Template System

**Variable syntax**:
- `$variable$` - Simple variable
- `$if(var)$...$endif$` - Conditional
- `$for(list)$...$endfor$` - Iteration
- `$partial("path")$` - Include partial template

**Template inheritance**:
1. Post content rendered from Markdown
2. Wrapped in `templates/post.html` (adds metadata)
3. Wrapped in `templates/default.html` (adds header/footer)
4. Header/footer are partial templates

**Common variables**:
- `$title$`, `$subtitle$`, `$author$`, `$date$`
- `$root$` (base URL)
- `$body$` (main content)
- `$updated$` (modification time)
- `$htmltitle$` (page title)

## Important Conventions

### File Naming

**Blog posts**: `posts/YYYY-MM-DD-slug.md`
- Date prefix required (used for sorting and date field)
- Slug should be descriptive, lowercase, hyphen-separated

**Pages**: `pages/name.md`
- No date prefix
- Routed explicitly in site.hs

**Templates**: `templates/name.html` or `templates/name.xml`
- HTML for web templates
- XML for sitemap
- TEX for LaTeX templates

### Content Conventions

**Markdown features**:
- Standard Pandoc Markdown
- YAML frontmatter for metadata
- Code blocks with syntax highlighting
- MathJax for equations: `$...$` (inline), `$$...$$` (block)
- Citations: `[@citation-key]` (requires bibliography)

**Image paths**:
- Absolute from root: `/images/filename.jpg`
- Referenced as `$root$/images/filename.jpg` in templates

**Internal links**:
- Use relative paths in Markdown
- `relativizeUrls` makes them relative after compilation

### CSS Organization

**style.css** (6KB):
- Color palette defined in comments
- Mobile-first responsive design
- Max width: 38em for readability
- Typography-focused (Charter serif)
- Custom styles for:
  - Code blocks (`.sourceCode`)
  - Blockquotes (`.blockquote`)
  - Links (hover effects)
  - Headers (consistent sizing)

**fonts.css**:
- Font-face declarations only
- WOFF2 format (modern browsers)
- Multiple weights: regular, bold, italic

### Haskell Code Style

**Conventions in site.hs**:
- OverloadedStrings pragma
- Qualified imports for clarity (Data.Map as M)
- Explicit type signatures for top-level functions
- Monadic composition with `>>=` for pipelines
- `mappend` for combining contexts/maps
- Pattern matching in where clauses

**GHC options**:
- `-threaded` for parallelism
- Warnings enabled during development

## Common Tasks

### Updating Content

#### Edit Existing Post
1. Edit `posts/YYYY-MM-DD-slug.md`
2. Save (auto-rebuilds in watch mode)
3. Preview at http://127.0.0.1:8082

#### Update CV
1. Edit `pages/cv.md`
2. Run `make cv` to regenerate PDF
3. Both HTML and PDF versions updated

#### Change Site Styling
1. Edit `css/style.css`
2. Save (auto-compresses and copies to _site/)
3. Hard refresh browser (Cmd/Ctrl+Shift+R)

### Modifying Site Structure

#### Add New Template
1. Create `templates/new-template.html`
2. Reference in site.hs: `loadAndApplyTemplate "templates/new-template.html" ctx`
3. Use template variables: `$variable$`

#### Change Homepage Layout
1. Edit `pages/main.md` (content)
2. Edit `templates/default.html` (structure)
3. Adjust `indexCtx` in site.hs if new variables needed (lines 161-166)

#### Modify Navigation
1. Edit `templates/header.html`
2. Add/remove links in navigation list
3. Restart `make watch` to see changes

### Working with Dependencies

#### Update Nix Dependencies
```bash
# Update all to latest
niv update

# Update specific dependency
niv update nixpkgs

# Pin to specific version
niv update nixpkgs -r <revision-hash>
```

#### Update Haskell Dependencies
1. Edit `dimitrije-website.cabal` (build-depends)
2. Run `cabal update` to fetch new package list
3. Run `cabal build` to build with new dependencies
4. May need to update `nix/overlays.nix` for Nix shell

#### Add New Haskell Dependency
1. Add to `dimitrije-website.cabal` build-depends:
   ```cabal
   build-depends: ...,
                  new-package >= x.y
   ```
2. Add to `nix/overlays.nix` ghcWithPackages:
   ```nix
   ghc = self.haskellPackages.ghcWithPackages (ps: with ps; [
     ...
     new-package
   ])
   ```
3. Rebuild: `cabal build`

### Debugging

#### Site Won't Build
```bash
# Clean and rebuild
make clean
cabal clean
cabal build
make rebuild

# Check Hakyll errors
site build  # More verbose output
```

#### Template Errors
- Check variable names match context definitions in site.hs
- Verify template syntax: `$if(var)$` requires `$endif$`
- Use `site rebuild` for full template recompilation

#### Syntax Highlighting Not Working
- Verify XML syntax file in `syntax/` directory
- Check XML is well-formed (common error: unclosed tags)
- Language name in fence must match `<language name="...">` in XML
- Restart `make watch` after adding new syntax files

#### CSS Changes Not Appearing
- Hard refresh browser (Cmd/Ctrl+Shift+R)
- Check `_site/css/style.css` was updated
- Verify `make watch` is running and detected change

### Testing Changes

#### Local Testing
```bash
make watch
# Open http://127.0.0.1:8082
# Test all pages: index, archive, posts, cv, contact
# Check RSS: http://127.0.0.1:8082/rss.xml
# Check sitemap: http://127.0.0.1:8082/sitemap.xml
```

#### Production Build Testing
```bash
make clean
make all
cd _site
python3 -m http.server 8000
# Test at http://localhost:8000
```

#### CV PDF Testing
```bash
make cv
# Open files/CV_Dimitrije_Radojevic.pdf
# Verify: citations, formatting, FontAwesome icons
```

## Nix Configuration Details

### Nix Architecture

**Three-layer setup**:

1. **shell.nix** (277 lines) - Development environment
   - Entry point for `nix-shell`
   - Provides all build tools and utilities
   - Sets up shell hooks and aliases

2. **default.nix** (5 lines) - Build convenience
   - Exposes `site` derivation
   - Can build with `nix-build -A site`

3. **nix/** directory - Configuration modules
   - `default.nix`: Imports nixpkgs with overlays
   - `overlays.nix`: Custom GHC with Haskell packages
   - `site.nix`: Hakyll site derivation
   - `sources.json`: Pinned dependency versions (Niv)
   - `sources.nix`: Dependency fetching logic

### Nix Commands

```bash
# Enter development shell
nix-shell

# Build site executable (creates result/ symlink)
nix-build -A site

# Update dependencies
niv update

# Check which packages will be built
nix-shell --dry-run

# Garbage collect old builds
nix-collect-garbage -d
```

### Pinned Dependencies

**Current pins** (nix/sources.json):
- **nixpkgs**: 24.11 branch
- **nixfiles**: Custom Neovim config
- **niv**: Dependency manager itself

All pins include SHA256 hashes for reproducibility.

## Troubleshooting

### Common Issues

#### "site: command not found"
**Solution**: Build the executable first
```bash
cabal build
# Executable is in dist-newstyle/build/.../site
# Or add to PATH via: cabal install
```

#### Nix shell fails to build
**Solution**: Update nixpkgs
```bash
niv update nixpkgs
nix-shell
```

#### Prettier not found
**Solution**: Ensure you're in nix-shell
```bash
exit  # Exit current shell
nix-shell
make watch
```

#### Port 8082 already in use
**Solution**: Kill existing process or use different port
```bash
# Find and kill process
lsof -ti:8082 | xargs kill -9

# Or change port in site.hs (previewPort = 8083)
```

#### PDF CV generation fails
**Solution**: Check XeLaTeX installation
```bash
which xelatex  # Should be in Nix store
nix-shell --run "make cv"
```

#### Syntax highlighting not appearing
**Solution**: Verify syntax.css generated
```bash
# Check if syntax.css exists
ls -la _site/css/syntax.css

# Rebuild
make rebuild
```

#### Deployment fails
**Solution**: Check SSH access to server
```bash
ssh dimitrije.website
# If works, then run:
make deploy
```

#### Changes not appearing in browser
**Solution**: Clear cache or hard refresh
- Chrome/Firefox: Cmd/Ctrl+Shift+R
- Safari: Cmd+Option+R
- Or clear browser cache completely

### Performance Tips

**Faster rebuilds**:
- Use `make watch` instead of `make rebuild`
- Hakyll caches compiled files in `_cache/`
- Only modified files are recompiled

**Faster Nix builds**:
- Pin dependencies with Niv (already done)
- Use binary cache (nixpkgs has official cache)
- Don't run `cabal clean` unless necessary

**Optimizing images**:
```bash
# Use WebP for better compression
magick input.jpg -quality 85 output.webp

# Optimize PNG
optipng -o7 image.png
```

## Additional Resources

### Documentation
- **Hakyll**: https://jaspervdj.be/hakyll/
- **Pandoc**: https://pandoc.org/MANUAL.html
- **Nix**: https://nixos.org/manual/nix/stable/
- **Skylighting**: https://github.com/jgm/skylighting

### Related Files
- Site source: site.hs
- Build config: dimitrije-website.cabal
- Nix config: shell.nix, nix/
- Build automation: Makefile
- Project info: README.md

### Git Workflow
- Use descriptive commit messages
- Branch naming: claude/* for automated branches
- Main branch: master (implied from commit history)
- Commits should be atomic and focused

### Contact
- **Author**: Dimitrije Radojević
- **Email**: me@dimitrije.website
- **Website**: https://dimitrije.website

---

**Last Updated**: 2025-11-23
**Hakyll Version**: 4.13+
**Nixpkgs**: 24.11
**Cabal Version**: 3.4
