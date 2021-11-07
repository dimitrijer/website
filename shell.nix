let
  pkgs = import ./nix/default.nix { };
  site = pkgs.haskellPackages.callPackage ./nix/site.nix { };
in
pkgs.mkShell {
  # GNU ls has different CLI options than Darwin ls.
  shellHooks = ''
    alias ll='ls -alh --color=auto'
    alias ls='ls -ah --color=auto'
  '';

  # Set UTF-8 locale.
  LANG = "C";
  LC_CTYPE = "UTF-8";

  buildInputs = [
    pkgs.ghc
    pkgs.haskell-language-server
    pkgs.nixpkgs-fmt
    site
  ];
}
