{ pkgs, config, lib, ... }:
let
  iosevka-term-slab = pkgs.iosevka-bin.override { variant = "sgr-iosevka-term-slab"; };
in
{
  programs.home-manager.enable = true;

  programs.zsh = {
    enable = true;

    enableCompletion = true;
    enableSyntaxHighlighting = true;
    enableVteIntegration = true;
    autocd = true;
    defaultKeymap = "emacs";

    plugins = [
      {
        name = "shrink-path";
        src = pkgs.oh-my-zsh + /share/oh-my-zsh/plugins/shrink-path;
      }
    ];

    initExtra = ''
      setopt prompt_subst
      PS1='$(shrink_path -f) %% '
      [[ -n "$SSH_TTY" ]] && PS1="$HOST $PS1"

      update_title() {
        print -Pn "\e]2;%m:%2~\a"
      }

      autoload -U add-zsh-hook
      add-zsh-hook -Uz chpwd update_title

      update_title

      bindkey '^p' _atuin_search_widget
    '';
    envExtra = builtins.readFile ./../zsh/.zshenv;
  };

  programs.atuin = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.exa = {
    enable = true;
    enableAliases = true;
  };

  programs.nix-index.enable = true;

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

  fonts.fontconfig.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacsPgtkNativeComp;
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
    nixUnstable
    nix-prefetch-git

    # Dhall
    dhall
    dhall-lsp-server

    # Coq
    coq

    # Fonts
    fira
    iosevka-term-slab
    julia-mono
    emacs-all-the-icons-fonts
    font-awesome_4
  ];
}
