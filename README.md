# dimitrije.website

This is source of https://dimitrije.website. The website is built with
[Hakyll](https://jaspervdj.be/hakyll/).

## Building and serving

You will need [Nix](https://github.com/NixOS/nix) package manager:

```bash
$ nix-shell
```

To serve locally, run `make watch`. This will rebuild static files and start
Hakyll local web server on port 8082 in watch mode. All local changes will be
detected, rebuilt and served.

To build CV PDF, run `make cv`.

To deploy, run `make deploy`.

## Licensing

Source code is released under [LGPL-3.0](https://www.gnu.org/licenses/lgpl-3.0.en.html).

Text, images and other non-source code content are released under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
