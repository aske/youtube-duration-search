{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, conduit-combinators, conduit-extra
      , gogol, gogol-core, gogol-youtube, interpolatedstring-perl6, lens
      , stdenv, text, time
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
