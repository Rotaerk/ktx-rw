{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let
  inherit (lib) makeLibraryPath;
  hs = haskell.packages.ghc843;
  tools = [
    hs.ghc
    hs.cabal-install
    hs.ghcid
  ];
  libraries = [
  ];
in
  pkgs.runCommand "shell" {
    buildInputs = tools ++ libraries;
    shellHook = ''
      LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${makeLibraryPath libraries}"
    '';
  } ""
