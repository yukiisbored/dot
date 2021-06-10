{ config, ... }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {
    overlays = [
      (import sources.emacs-overlay)
    ];
  };

  comma = import (pkgs.fetchFromGitHub {
      owner = "Shopify";
      repo = "comma";
      rev = "4a62ec17e20ce0e738a8e5126b4298a73903b468";
      sha256 = "0n5a3rnv9qnnsrl76kpi6dmaxmwj1mpdd2g0b4n1wfimqfaz6gi1";
  }) {};
in
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
    comma
  ];

  services.lorri.enable = true;

  home.stateVersion = "21.03";
}
