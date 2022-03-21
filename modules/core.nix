{ pkgs, config, lib, ... }:

let
  nodePackages' = import ./../node/default.nix { inherit pkgs; };

  iosevka-etoile = pkgs.fetchzip {
    name = "iosevka-etoile";
    url = "https://github.com/be5invis/Iosevka/releases/download/v11.0.1/ttf-iosevka-etoile-11.0.1.zip";
    sha256 = "UKEfkZQU9o1Hk2SVxdMMtLUmjYgIaf6mTvfTb5zIKTw=";
    postFetch = ''
      mkdir -p $out/share/fonts
      unzip $downloadedFile -d $out/share/fonts/truetype
    '';
  };

  iosevka-term-slab = pkgs.fetchzip {
    name = "iosevka-term-slab";
    url = "https://github.com/be5invis/Iosevka/releases/download/v11.0.1/ttf-iosevka-term-slab-11.0.1.zip";
    sha256 = "czgr/xjTnBLXuBeYkRDxMPISZJPb4GGlgh0J/6hYAgk=";
    postFetch = ''
      mkdir -p $out/share/fonts
      unzip $downloadedFile -d $out/share/fonts/truetype
    '';
  };
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

    nix-direnv = {
      enable = true;
      enableFlakes = true;
    };
  };

  programs.git = {
    enable = true;
    userName = "Muhammad Kaisar Arkhan (Yuki)";
    userEmail = "hi@yukiisbo.red";
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacsPgtkGcc;
      config = ./../emacs/.emacs.d/init.el;
      alwaysEnsure = true;

      override = epkgs: lib.filterAttrs (n: v: n != "tree-sitter" && n != "tree-sitter-langs") epkgs;
    };
    extraConfig = builtins.readFile ./../emacs/.emacs.d/init.el;
  };

  services.emacs.enable = true;

  home.file = {
    ".emacs.d/assets".source = ./../emacs/.emacs.d/assets;
    ".emacs.d/site-lisp/ios-config-mode.el".source = builtins.fetchurl {
      url = "https://raw.githubusercontent.com/nibrahim/IOS-config-mode/master/ios-config-mode.el";
      sha256 = "17nzmrbaarkzv2cyqjzgssibhmvqlq10jfraa7zkgnazybk3nd9a";
    };
  };

  fonts.fontconfig.enable = true;

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

    # Python
    pipenv
    poetry
    nodePackages.pyright

    # JavaScript / Node.js
    nodejs
    yarn
    nodePackages.npm
    nodePackages.typescript-language-server
    nodePackages.node2nix

    # PureScript
    spago
    nodePackages'.purescript-language-server

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

    # OCaml
    dune_2
    opam
    ocamlPackages.merlin
    ocamlPackages.ocp-indent
    ocamlPackages.utop

    # Coq
    coq

    # Fonts
    iosevka-term-slab
    iosevka-etoile
    emacs-all-the-icons-fonts
    font-awesome_4
  ];

  services.lorri.enable = true;
}
