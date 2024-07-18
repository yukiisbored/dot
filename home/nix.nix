{ pkgs, ... }:
{
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  home.packages = with pkgs; [
    devbox
    direnv
    cachix
    nix-prefetch-git
  ];
}
