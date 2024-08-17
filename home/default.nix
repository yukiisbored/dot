{ pkgs, config, lib, isLinux, isDarwin, ... }:
{
  imports = [
    ./zsh
    ./git.nix
    ./nix.nix

    ./editors.nix
    ./prog.nix
  ];
  
  programs.home-manager.enable = true;
  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    # General utilities
    silver-searcher
    sqlite
    httpie
    asciinema
    graphviz
    croc
    pwgen
    yt-dlp
    jq
    tmate
    gh
    mosh
    lazygit

    # Encryption
    gnupg
    age
    cryfs

    # Cloud
    kubectl
    kubectx
    kubernetes-helm
    scaleway-cli
    kind
    doctl
    netlify-cli
    krew

    # Fonts
    jetbrains-mono
  ] ++ lib.optionals isLinux [
    google-cloud-sdk
    tomb
  ];
}
