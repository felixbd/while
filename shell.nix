{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name="dev-environment";

  buildInputs = with pkgs; [
    stack
    ghc
    ghcid
    hlint

    pre-commit
  ];

  shellHook = ''
    echo "Start developing..."
  '';
}
