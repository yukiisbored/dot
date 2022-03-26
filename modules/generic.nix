{ lib, ... }:
{
  xdg = {
    enable = true;
    mime.enable = true;
  };

  targets.genericLinux.enable = true;

  programs.bash.enable = true;

  home.activation = {
    hackApplications = lib.hm.dag.entryAfter ["linkGeneration"] ''
      $DRY_RUN_CMD mkdir $VERBOSE_ARG -p $HOME/.local/share/applications/nix/
      $DRY_RUN_CMD rm $VERBOSE_ARG -f $HOME/.local/share/applications/nix/*.desktop
      $DRY_RUN_CMD [ -d $HOME/.nix-profile/share/applications ] && cp $VERBOSE_ARG -L $HOME/.nix-profile/share/applications/*.desktop $HOME/.local/share/applications/nix/
      $DRY_RUN_CMD chmod $VERBOSE_ARG -R u+w,a+x $HOME/.local/share/applications/nix/
    '';
  };
}
