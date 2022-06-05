let
  pkgs = import ./nix/default.nix { };
  site = pkgs.haskellPackages.callPackage ./nix/site.nix { };
  sources = import ./nix/sources.nix;
  nixfiles = import sources.nixfiles { };
  neovim = nixfiles.neovim {
    pkgs = pkgs;
    withHaskell = true;
    withWriting = true;
  };
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive)
      scheme-small
      fontawesome
      sectsty
      enumitem
      titling
      wrapfig;
  });
in
pkgs.mkShell {
  # GNU ls has different CLI options than Darwin ls.
  shellHooks = ''
    alias ll='ls -alh --color=auto'
    alias ls='ls -ah --color=auto'
    alias vim='nvim'
  '';

  buildInputs = [
    pkgs.ghc
    # For converting images
    pkgs.imagemagick
    # For development
    pkgs.haskell-language-server
    neovim
    pkgs.nixpkgs-fmt
    pkgs.haskellPackages.ormolu
    # For CV
    tex
    # Main site package
    site
  ];
}
