{ pkgs, config, lib, isLinux, ... }:
{

  imports = [
    ../modules/base.nix

    ../modules/zsh
    ../modules/git.nix
    ../modules/nix.nix

    ../modules/neovim.nix
    ../modules/emacs.nix
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
    cryfs

    # Cloud
    kubectl
    kubectx
    konfig
    kubernetes-helm
    helmfile
    scaleway-cli
    kind
    doctl
    netlify-cli
  ] ++ lib.optionals isLinux [
    google-cloud-sdk
    gke-gcloud-auth-plugin
    tomb
  ];
}
