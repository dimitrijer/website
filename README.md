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
