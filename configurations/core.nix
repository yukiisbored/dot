{ pkgs, config, lib, isLinux, ... }:
{

  imports = [
    ../modules/base.nix

    ../modules/zsh
    ../modules/git.nix
    ../modules/nix.nix

    ../modules/neovim.nix
    ../modules/emacs
    ../modules/helix.nix

    ../modules/prog.nix
  ];

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
    ngrok
    gh

    # Encryption
    trezor_agent
    gnupg
    age

    # Cloud
    kubectl
    kubectx
    konfig
    kubernetes-helm
    helmfile
    google-cloud-sdk
    scaleway-cli
    kind
  ] ++ lib.optionals isLinux [
    tomb
    gke-gcloud-auth-plugin
  ];
}
