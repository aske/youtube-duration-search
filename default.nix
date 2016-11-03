{ mkDerivation, base, conduit-combinators, conduit-extra, gogol
, gogol-core, gogol-youtube, interpolatedstring-perl6, lens, stdenv
, text, time
}:
mkDerivation {
  pname = "youtube-duration-search";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base conduit-combinators conduit-extra gogol gogol-core
    gogol-youtube interpolatedstring-perl6 lens text time
  ];
  license = stdenv.lib.licenses.bsd3;
}
