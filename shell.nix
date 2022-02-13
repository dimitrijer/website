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
in
pkgs.mkShell {
  # GNU ls has different CLI options than Darwin ls.
  shellHooks = ''
    alias ll='ls -alh --color=auto'
    alias ls='ls -ah --color=auto'
    alias vim='nvim'
  '';

  # Set UTF-8 locale.
  LANG = "C";
  LC_CTYPE = "UTF-8";

  buildInputs = [
    pkgs.ghc
    pkgs.haskell-language-server
    pkgs.nixpkgs-fmt
    pkgs.ormolu
    # For converting images
    pkgs.imagemagick
    neovim
    site
  ];
}
