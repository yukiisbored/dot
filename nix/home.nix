{ pkgs, config, ... }:

{
  programs.home-manager.enable = true;

  home.username = "yuki";
  home.homeDirectory = "/home/yuki";
  home.packages = with pkgs; [
    emacsGcc

    tmux
    silver-searcher
    sqlite
    httpie
    age

    pipenv
    poetry
    python38Packages.python-language-server

    nodejs-12_x
    nodePackages.typescript-language-server
    nodePackages.node2nix

    kubectl
    kubectx
    google-cloud-sdk

    haxe

    direnv
    niv
    cachix
  ];

  services.lorri.enable = true;
}
