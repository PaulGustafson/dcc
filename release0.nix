let
  pkgs = import <nixpkgs> { };

in
  { dcc = pkgs.haskellPackages.callPackage ./dcc.nix { };
  }