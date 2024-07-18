{ pkgs, ... }:
{
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  home.packages = with pkgs; [
    devbox
    devenv
    direnv
    niv
    cachix
    nix-prefetch-git
  ];
}
