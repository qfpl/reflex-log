{ mkDerivation, base, binary, bytestring, containers, directory
, filepath, lens, mtl, reflex, reflex-dom, sqlite-simple, stdenv
, stm, text
}:
mkDerivation {
  pname = "reflex-log";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers directory filepath lens mtl
    reflex sqlite-simple stm text
  ];
  executableHaskellDepends = [ base reflex reflex-dom text ];
  license = stdenv.lib.licenses.bsd3;
}
