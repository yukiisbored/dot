{ pkgs, ... }:
{
  imports = [
    ./zsh
  ];

  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    silver-searcher-ng
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
    nixd
    nil
  ];

  nix = {
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
    lfs.enable = true;

    settings = {
      init.defaultBranch = "main";
      user.name = "Yuki Langley";
      user.email = "hi@yukiisbo.red";
    };
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };
}
