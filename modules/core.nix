{ pkgs, config, lib, ... }:
{
  imports = [
    ./base.nix

    ./zsh.nix
    ./git.nix
    ./nix.nix

    ./neovim.nix 
    ./emacs.nix 

    ./prog.nix
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
