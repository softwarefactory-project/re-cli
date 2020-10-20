{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = [ pkgs.nodejs pkgs.bs-platform ];

    shellHook =
      ''
      mkdir -p node_modules
      ln -sf ${pkgs.bs-platform} node_modules/bs-platform
      '';
  }
