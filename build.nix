{ mkDerivation, aeson, base, bytestring, hashmap, lucid, random
, servant, servant-lucid, servant-server, stdenv, text, uri-encode
, wai, warp
}:
mkDerivation {
  pname = "lmrs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSharedExecutables = false;
  executableHaskellDepends = [
    aeson base bytestring hashmap lucid random servant servant-lucid
    servant-server text uri-encode wai warp
  ];
  homepage = "https://github.com/dgonyeo/lmrs#readme";
  license = stdenv.lib.licenses.bsd3;
}
