{ lib, ... }:
{
  xdg = {
    enable = true;
    mime.enable = true;
  };

  targets.genericLinux.enable = true;

  programs.bash.enable = true;
}
