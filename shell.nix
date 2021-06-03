{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let
  inherit (lib) makeLibraryPath;
  hs = haskell.packages.ghc884;
  tools = [
    hs.ghc
    hs.cabal-install
    hs.ghcid
  ];
  libraries = [
  ];
  libraryPath = "${makeLibraryPath libraries}";
in
  pkgs.runCommand "shell" {
    buildInputs = tools ++ libraries;
    shellHook = ''
      export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${libraryPath}"
      export LIBRARY_PATH="${libraryPath}"
    '';
  } ""
