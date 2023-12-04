{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name="dev-environment";

  buildInputs = with pkgs; [
    stack
    ghc
    hlint
  ];

  shellHook = ''
    echo "Start developing..."
  '';
}
