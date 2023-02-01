{ pkgs, config, lib, ... }:
{
  programs.home-manager.enable = true;

  programs.zsh = {
    enable = true;

    enableAutosuggestions = true;
    enableCompletion = true;
    enableVteIntegration = true;
    autocd = true;
    defaultKeymap = "emacs";
    history = {
      extended = true;
      path = "${config.home.homeDirectory}/.histfile";
    };

    plugins = [
      {
        name = "fast-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zdharma-continuum";
          repo = "fast-syntax-highlighting";
          rev = "770bcd986620d6172097dc161178210855808ee0";
          sha256 = "T4k0pbT7aqLrIRIi2EM15LXCnpRFHzFilAYfRG6kbeY=";
        };
      }
      {
        name = "shrink-path";
        src = pkgs.oh-my-zsh + /share/oh-my-zsh/plugins/shrink-path;
      }
      {
        name = "zsh-fzf-history-search";
        src = pkgs.fetchFromGitHub {
          owner = "joshskidmore";
          repo = "zsh-fzf-history-search";
          rev = "f2432b240c40cff889aab3f10272b8466fb3d9ab";
          sha256 = "+reiUhVneaz2u170cltpYtYaHhu9wvaZuhf8TdJIrGs=";
        };
      }
      {
        name = "fzf-tab";
        src = pkgs.zsh-fzf-tab + /share/fzf-tab;
      }
    ];

    initExtra = builtins.readFile ./../zsh/.zshrc;
    envExtra = builtins.readFile ./../zsh/.zshenv;
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.exa = {
    enable = true;
    enableAliases = true;
  };

  programs.nix-index = {
    enable = true;
    enableZshIntegration = false;
  };

  programs.scmpuff = {
    enable = true;
    enableZshIntegration = true;
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

  programs.helix = {
    enable = true;
    settings = {
      editor.cursor-shape = {
        insert = "bar";
        normal = "block";
        select = "underline";
      };
    };
  };

  fonts.fontconfig.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacsPgtk;
    extraPackages = epkgs: [ epkgs.vterm ];
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
    croc
    (aspellWithDicts (ds: with ds; [ en en-computers en-science fr ]))
    languagetool
    wakatime

    # File storage
    git-lfs
    git-crypt
    trezor_agent
    gnupg
    age

    # Cloud
    kubectl
    kubectx
    google-cloud-sdk
    gke-gcloud-auth-plugin

    # Python
    pipenv
    poetry
    nodePackages.pyright

    # JavaScript / Node.js
    nodePackages.typescript-language-server
    nodePackages.node2nix
    nodePackages.gatsby-cli

    # OCaml / Reason
    nodePackages.esy

    # Haskell
    haskellPackages.brittany

    # Nix
    direnv
    niv
    cachix
    nixUnstable
    nix-prefetch-git

    # Dhall
    dhall
    dhall-lsp-server

    # Coq
    coq

    # Fonts
    fira
    julia-mono
    emacs-all-the-icons-fonts
    font-awesome_4
  ];
}
