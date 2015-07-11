{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:
nixpkgs.haskellPackages.callPackage ./base32string.nix { }
