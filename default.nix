{ mkDerivation, aeson, ansi-terminal, base, bytestring, containers
, directory, filepath, language-nix, lens, lens-aeson, lifted-async
, lifted-base, monad-control, mtl, network-uri
, optparse-applicative, process-extras, semver-range, stdenv, text
, transformers, trifecta, unordered-containers, utf8-string, wreq
}:
mkDerivation {
  pname = "npm2nix-ng";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson ansi-terminal base bytestring containers directory filepath
    language-nix lens lens-aeson lifted-async lifted-base monad-control
    mtl network-uri optparse-applicative process-extras semver-range
    text transformers trifecta unordered-containers utf8-string wreq
  ];
  license = stdenv.lib.licenses.mit;
}
