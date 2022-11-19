# dimitrije.website

This is source of https://dimitrije.website. The website is built with
[Hakyll](https://jaspervdj.be/hakyll/).

## Building and serving

You will need Nix:

```bash
$ nix-shell
```

To serve locally, run `make watch`. This will rebuild static files and start
Hakyll local web server on port 8082 in watch mode. All local changes will be
detected, rebuilt and served.

To build CV PDF, run `make cv`.

To deploy, run `make deploy`.
