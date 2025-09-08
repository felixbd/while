{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name="dev-environment";

  buildInputs = with pkgs; [
    stack
    ghc
    hlint

    #(python313.withPackages(ps: with ps; [
    #  pre-commit
    #  pre-commit-hooks
    #]))

    pre-commit
  ];

  shellHook = ''
    # python -m pre-commit --version
    # pre-commit --version
    echo "Start developing..."
  '';
}
