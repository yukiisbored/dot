{ pkgs, config, ... }:

let
  nodePackages' = import ./../node/default.nix { inherit pkgs; };

  emacs = pkgs.emacsWithPackagesFromUsePackage {
    package = pkgs.emacsGcc;
    config = ./../emacs/.emacs.d/init.el;
    alwaysEnsure = true;
  };

  ghc = pkgs.haskellPackages.ghcWithPackages
    (p: with p; [ lens mtl xmonad xmonad-contrib xmonad-extras ]);
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
        name = "fast-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zdharma";
          repo = "fast-syntax-highlighting";
          rev = "817916dfa907d179f0d46d8de355e883cf67bd97";
          sha256 = "rHdOnA0Y/i/ISDlriGrGDXr5rD2nZ4T7iuG7PFUVIFQ=";
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

  programs.nix-index = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.git = {
    enable = true;
    userName = "Muhammad Kaisar Arkhan (Yuki)";
    userEmail = "hi@yukiisbo.red";
  };

  home.file = {
    ".emacs.d/init.el".source = ./../emacs/.emacs.d/init.el;
    ".emacs.d/assets".source = ./../emacs/.emacs.d/assets;
    ".emacs.d/site-lisp/ligature.el".source = builtins.fetchurl {
      url = "https://raw.githubusercontent.com/mickeynp/ligature.el/45132323de4f42d3273307f569c2a8418020a46f/ligature.el";
      sha256 = "0cc8j8zv7s3d4dv3sb9xdaygnqc20v7jiqcpvi8d4gjifbbcmbhq";
    };
  };

  home.packages = with pkgs; [
    emacs

    # General utilities
    tmux
    silver-searcher
    sqlite
    httpie
    asciinema

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
    python38
    python38Packages.python-lsp-server

    # JavaScript / Node.js
    nodejs-12_x
    nodePackages.typescript-language-server
    nodePackages.node2nix

    # PureScript
    purescript
    spago
    nodePackages'.purescript-language-server

    # Haskell
    ghc
    cabal-install
    haskell-language-server
    haskellPackages.brittany

    # Haxe
    haxe

    # Nix
    direnv
    niv
    cachix
    comma

    # Dhall
    dhall
    dhall-lsp-server

    # C/C++
    gcc
    gnumake
    cmake
    libtool

    # OCaml
    dune_2
    opam
    ocamlPackages.merlin
    ocamlPackages.ocp-indent
    ocamlPackages.utop

    # Java
    jdk11
  ];

  services.lorri.enable = true;
}
