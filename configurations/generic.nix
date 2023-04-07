{ lib, ... }:
{
  imports = [
    ./core.nix
  ];

  xdg = {
    enable = true;
    mime.enable = true;
  };

  targets.genericLinux.enable = true;

  programs.bash.enable = true;
}
