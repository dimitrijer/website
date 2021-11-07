{ sources ? import ./sources.nix }:

import sources.nixpkgs {
  overlays = [ (import ./overlays.nix) ];
  config = { };
}
