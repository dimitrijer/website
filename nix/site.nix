{ mkDerivation, base, hakyll, lib, pandoc, time }:
mkDerivation {
  pname = "dimitrije-website";
  version = "0.1.0.0";
  src = (lib.sourceByRegex ../. [
    "^.*\\.hs$"
    "^.*\\.cabal$"
    "^LICENSE$"
    "^LICENSE-CONTENT$"
  ]);
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll pandoc time ];
  homepage = "https://dimitrije.website";
  license = with lib.licenses; [ lgpl3 cc-by-40 ];
}
