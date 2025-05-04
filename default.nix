{ pkgs ? import ./nix/default.nix { } }:

{
  site = pkgs.haskellPackages.callPackage ./nix/site.nix { };
}
