{ pkgs, config, ... }:

{
  fonts.fontconfig.enable = true;

  xsession = {
    enable = true;

    pointerCursor = {
      package = pkgs.vanilla-dmz;
      name    = "Vanilla-DMZ-AA";
      size    = 16;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;

      config = ./../xmonad/.xmonad/xmonad.hs;
    };

    initExtra = ''
      ${pkgs.nitrogen}/bin/nitrogen --restore &
    '';
  };

  services.dunst = {
    enable = true;

    settings = {
      global = {
        geometry = "384x16-24+45";
        transparency = 5;
        frame_color = "#000000";
        frame_width = 16;

        font = "Fira Sans 12";

        markup = "full";
        format = "<b>%s</b>\\n%b";

        vertical_alignment = "center";
        word_wrap = true;
        ignore_newline = false;
      };

      urgency_normal = {
        background = "#000000";
        foreground = "#ffffff";
        timeout = 10;
      };
    };
  };

  services.picom = {
    enable = true;
    shadow = true;
  };

  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.betterlockscreen}/bin/betterlockscreen --lock";
  };

  services.flameshot.enable = true;

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
    theme = "sidebar";

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
    migu
    emacs-all-the-icons-fonts
    font-awesome_4

    xmobar
    betterlockscreen
    nitrogen
  ];
}
