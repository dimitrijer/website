{ mkDerivation, base, hakyll, lib, pandoc, pandoc-types, time }:
mkDerivation {
  pname = "dimitrije-website";
  version = "0.1.0.0";
  src = (lib.sourceByRegex ../. [
    "^.*\\.hs$"
    "^.*\\.cabal$"
    "^LICENSE$"
  ]);
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll pandoc pandoc-types time ];
  homepage = "https://dimitrije.website";
  license = lib.licenses.lgpl3;
}
