{ mkDerivation, aeson, base, bytestring, hashmap, lucid, random
, servant, servant-client, servant-lucid, servant-server, stdenv
, text, uri-encode, warp
}:
mkDerivation {
  pname = "lmrs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSharedExecutables = false;
  executableHaskellDepends = [
    aeson lucid servant-client servant-lucid
    base bytestring hashmap random servant servant-server text
    uri-encode warp
  ];
  homepage = "https://github.com/dgonyeo/lmrs#readme";
  license = stdenv.lib.licenses.bsd3;
}
