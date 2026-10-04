{ sources ? import ./sources.nix }:

import sources.nixpkgs {
  overlays = [ (import ./overlays.nix) ];
  config = {
    # Pulled in by the neovim config from nixfiles; marked unfree in nixpkgs
    # because upstream has no license.
    allowUnfreePredicate = pkg: builtins.elem (pkg.pname or (builtins.parseDrvName pkg.name).name) [
      "vim-highlightedyank"
    ];
  };
}
