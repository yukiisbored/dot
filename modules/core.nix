{ pkgs, config, lib, ... }:
let
  iosevka-term-slab = pkgs.iosevka-bin.override { variant = "sgr-iosevka-term-slab"; };
in
{
  programs.home-manager.enable = true;

  programs.zsh = {
    enable = true;

    initExtra = builtins.readFile ./../zsh/.zshrc;
    envExtra = builtins.readFile ./../zsh/.zshenv;

    shellAliases = {
      flake = "nix flake";
    };

    plugins = [
      {
        name = "zsh-history-substring-search";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-history-substring-search";
          rev = "4abed97b6e67eb5590b39bcd59080aa23192f25d";
          sha256 = "8kiPBtgsjRDqLWt0xGJ6vBBLqCWEIyFpYfd+s1prHWk=";
        };
      }
      {
        name = "zsh-autosuggestions";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-autosuggestions";
          rev = "a411ef3e0992d4839f0732ebeb9823024afaaaa8";
          sha256 = "KLUYpUu4DHRumQZ3w59m9aTW6TBKMCXl2UcKi4uMd7w=";
        };
      }
      {
        name = "zsh-completions";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-completions";
          rev = "d4511c23659381b56dec8be8c8553b7ff3dc5fd8";
          sha256 = "OOMabAhRcgs7YpCx+g6yIqTHDMwMueBD+s7P+WCdHPk=";
        };
      }
      {
        name = "zsh-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-syntax-highlighting";
          rev = "c7caf57ca805abd54f11f756fda6395dd4187f8a";
          sha256 = "MeuPqDeJpbJi2hT7VUgyQNSmDPY/biUncvyY78IBfzM=";
        };
      }
    ];
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;

    nix-direnv.enable = true;
  };

  programs.git = {
    enable = true;
    userName = "Yuki Langley";
    userEmail = "hi@yukiisbo.red";
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
  };

  fonts.fontconfig.enable = true;

  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ../doom/.doom.d;
    emacsPackage = pkgs.emacsPgtkNativeComp;
  };

  home.packages = with pkgs; [
    # General utilities
    tmux
    silver-searcher
    sqlite
    httpie
    asciinema
    graphviz
    shairport-sync
    comma
    ripgrep
    fd
    emacsql-sqlite
    croc

    # File storage
    git-lfs
    git-crypt
    trezor_agent
    gnupg
    age

    # Cloud
    kubectl
    kubectx
    kubectl-modify-secret
    google-cloud-sdk

    # Godot
    python39Packages.gdtoolkit

    # Python
    pipenv
    poetry
    nodePackages.pyright

    # JavaScript / Node.js
    nodePackages.typescript-language-server
    nodePackages.node2nix
    nodejs-14_x

    # Haskell
    haskellPackages.brittany

    # Nix
    direnv
    niv
    cachix
    nixFlakes
    nix-prefetch-git

    # Dhall
    dhall
    dhall-lsp-server

    # Coq
    coq

    # Fonts
    fira
    iosevka-term-slab
    emacs-all-the-icons-fonts
    font-awesome_4
  ];
}
