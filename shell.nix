{ pkgs ? import <nixos> {}
}:

with pkgs;

stdenv.mkDerivation {
  name = "lev";
  buildInputs = [
    ghc
    stack
    zlib
];
}
