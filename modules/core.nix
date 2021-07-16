{ pkgs, config, ... }:

{
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    emacsGcc

    tmux
    silver-searcher
    sqlite
    httpie

    trezor_agent
    gnupg
    age

    pipenv
    poetry
    python38
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

    gcc
    gnumake
    cmake
    libtool
  ];

  services.lorri.enable = true;
}
