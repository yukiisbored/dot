{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # Python
    pipenv
    poetry
    nodePackages.pyright

    # JavaScript / Node.js
    nodejs-18_x
    nodePackages.typescript-language-server
    nodePackages.node2nix
    nodePackages.gatsby-cli
    deno

    # OCaml / Reason
    nodePackages.esy

    # Dhall
    dhall
    dhall-lsp-server

    # Coq
    coq
  ];
}
