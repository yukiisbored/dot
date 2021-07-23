{ pkgs, config, ... }:

let
  nodePackages' = import ./../node/default.nix { inherit pkgs; };
in
{
  programs.home-manager.enable = true;

  xdg.configFile."nix/nix.conf".source = ./../nix/nix.conf;

  home.packages = with pkgs; [
    emacsGcc

    tmux
    silver-searcher
    sqlite
    httpie

    git-crypt
    trezor_agent
    gnupg
    age

    pipenv
    poetry
    python38
    python38Packages.python-language-server

    nodejs-12_x
    nodePackages.typescript-language-server
    nodePackages.node2nix

    purescript
    spago
    nodePackages'.purescript-language-server

    ghc
    cabal-install
    haskell-language-server

    kubectl
    kubectx
    google-cloud-sdk

    haxe

    direnv
    niv
    cachix
    comma

    dhall
    dhall-lsp-server

    gcc
    gnumake
    cmake
    libtool
  ];

  services.lorri.enable = true;
}
