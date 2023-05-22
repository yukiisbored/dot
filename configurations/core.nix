{ pkgs, config, lib, ... }:
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
    tmux
    silver-searcher
    sqlite
    httpie
    asciinema
    graphviz
    croc
    pwgen
    yt-dlp
    jq
    tomb

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
    gke-gcloud-auth-plugin
    scaleway-cli
    kind
  ];
}
