{ config, pkgs, ... }:

let
   comma = import ( pkgs.fetchFromGitHub {
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
  home.packages = [
    pkgs.tmux

    pkgs.emacs
    pkgs.silver-searcher

    pkgs.pipenv
    pkgs.poetry
    pkgs.python38Packages.python-language-server

    pkgs.nodejs-12_x
    pkgs.nodePackages.typescript-language-server

    pkgs.kubectl
    pkgs.kubectx
    pkgs.google-cloud-sdk

    pkgs.sqlite

    pkgs.haxe

    pkgs.direnv
    comma
  ];

  services.lorri.enable = true;

  home.stateVersion = "21.03";
}
