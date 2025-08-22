{ pkgs, ... }:
{
  imports = [
    ./zsh
  ];
  
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    silver-searcher
    sqlite
    graphviz
    croc
    pwgen
    yt-dlp
    jq
    tmate
    gh
    mosh
    lazygit
    zellij
  ];

  nix = {
    package = pkgs.nix;

    settings = {
      substituters = [
        "https://nix-community.cachix.org"
      ];

      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };

  programs.git = {
    enable = true;
    userName = "Yuki Langley";
    userEmail = "hi@yukiisbo.red";

    extraConfig = {
      init.defaultBranch = "main";
    };

    lfs.enable = true;
  };

  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    extraPackages = with pkgs; [
      git
      lazygit

      findutils
      fzf
      ripgrep
      fd

      lua-language-server
      stylua

      nodejs_22
    ];
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };
}
