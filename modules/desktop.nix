{ pkgs, config, ... }:

{
  fonts.fontconfig.enable = true;

  xsession = {
    enable = true;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;

      config = ./../xmonad/.xmonad/xmonad.hs;
    };

    initExtra = ''
      ${pkgs.xfce.xfce4-power-manager}/bin/xfce4-power-manager &
      ${pkgs.nitrogen}/bin/nitrogen --restore &
    '';
  };

  services.grobi = {
    enable = true;

    rules = [
      {
        name = "Home";
        outputs_connected = [ "DP-1-2" ];
        configure_single = "DP-1-2";
        primary = true;
        atomic = true;
        execute_after = [
          "${pkgs.nitrogen}/bin/nitrogen --restore"
        ];
      }
      {
        name = "Mobile";
        outputs_disconnected = [ "DP-1-2" ];
        configure_single = "eDP-1";
        primary = true;
        atomic = true;
        execute_after = [
          "${pkgs.nitrogen}/bin/nitrogen --restore"
        ];
      }
    ];
  };

  services.xscreensaver.enable = true;

  programs.kitty = {
    enable = true;

    font = {
      name = "Fira Code";
      size = 12;
    };

    settings = {
      foreground = "#000000";
      background = "#ffffff";

      window_padding_width  = "16";
    };
  };

  programs.rofi = {
    enable = true;

    font = "Fira Code 12";
    theme = "Arc";

    extraConfig = {
      modi = "drun";
    };
  };

  xdg.configFile."xmobar/.xmobarrc".source = ./../xmonad/.config/xmobar/.xmobarrc;

  home.packages = with pkgs; [
    ungoogled-chromium
    thunderbird

    zoom-us
    element-desktop
    tdesktop

    gimp
    darktable

    jetbrains.idea-community

    nextcloud-client

    libreoffice-still

    fira
    fira-code
    emacs-all-the-icons-fonts

    xmobar
    xfce.xfce4-power-manager
    xfce.xfce4-screenshooter
    xscreensaver
    nitrogen
  ];
}
