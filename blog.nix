{ mkDerivation, base, containers, filepath, hakyll, pandoc
, pandoc-types, stdenv
}:
mkDerivation {
  pname = "blog";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers filepath hakyll pandoc pandoc-types
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
