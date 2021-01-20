{ config, pkgs, ... }:

{
  programs.home-manager.enable = true;

  home.username = "yuki";
  home.homeDirectory = "/home/yuki";
  home.packages = with pkgs; [
    # Physiological needs
    emacs
    git
    stow
    tmux

    # Docker
    docker
    docker-compose

    # Kubernetes
    kubectl
    kubectx
    kubespy
    kubernetes-helm
    kubeval
    kustomize
    kind

    # Ansible
    ansible
    ansible-lint

    # Cloud providers
    google-cloud-sdk
    awscli

    # Python
    python3
    poetry
    python38Packages.flake8

    # Web development
    nodejs-12_x

    # Haskell
    ghc
    haskell-language-server
    cabal-install
    hlint

    # Rust
    rustup

    # Elixir/Erlang
    elixir
    erlang

    # Go
    go

    # Java
    jdk11
    gradle
    maven
  ];

  home.stateVersion = "21.03";
}
