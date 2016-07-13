{ mkDerivation, aeson, ansi-wl-pprint, base, base64-bytestring
, bytestring, containers, directory, exceptions, filepath
, HsOpenSSL, http-client-openssl, lens, lens-aeson, lifted-async
, lifted-base, memory-cache, monad-control, mtl, network-uri
, optparse-applicative, process, process-extras, semver-range
, stdenv, temporary, text, transformers, trifecta
, unordered-containers, utf8-string, wreq
}:
mkDerivation {
  pname = "npm2nix-ng";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson ansi-wl-pprint base base64-bytestring bytestring containers
    directory exceptions filepath HsOpenSSL http-client-openssl lens
    lens-aeson lifted-async lifted-base memory-cache monad-control mtl
    network-uri optparse-applicative process process-extras
    semver-range temporary text transformers trifecta
    unordered-containers utf8-string wreq
  ];
  license = stdenv.lib.licenses.mit;
}
