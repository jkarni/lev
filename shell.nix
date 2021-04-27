{ pkgs ? import <nixos> {}
}:

with pkgs;

stdenv.mkDerivation {
  name = "lev";
  buildInputs = [
    haskell.compiler.ghc8104
    stack
    zlib
];
}
