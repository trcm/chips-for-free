{ mkDerivation, aeson, base, bytestring, containers, polysemy
, polysemy-plugin, stdenv, text
}:
mkDerivation {
  pname = "chip-free";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers polysemy polysemy-plugin text
  ];
  executableHaskellDepends = [ base polysemy polysemy-plugin ];
  license = stdenv.lib.licenses.bsd3;
}
