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
        outputs_connected = [ "DP-1" ];
        configure_single = "DP-1";
        primary = true;
        atomic = true;
        execute_after = [
          "${pkgs.nitrogen}/bin/nitrogen --restore"
        ];
      }
      {
        name = "Mobile";
        outputs_disconnected = [ "DP-1" ];
        configure_single = "LVDS-1";
        primary = true;
        atomic = true;
        execute_after = [
          "${pkgs.nitrogen}/bin/nitrogen --restore"
        ];
      }
    ];
  };

  programs.alacritty = {
    enable = true;

    settings = {
      env = {
        TERM = "xterm-256color";
        WINIT_HIDPI_FACTOR = "1";
        WINIT_X11_SCALE_FACTOR = "1";
      };

      font = {
        normal.family = "Fira Code";
        size = 12.0;
      };

      colors = {
        primary = {
          foreground = "#2a2a2a";
          background = "#ffffff";
          bright_foreground = "#2a2a2a";
        };

        normal = {
          black =   "#2a2a2a";
          red =     "#bf1a08";
          green =   "#50a14f";
          yellow =  "#986801";
          blue =    "#4078f2";
          magenta = "#7807b5";
          cyan =    "#0184bc";
          white =   "#ffffff";
        };

        bright = {
          black =   "#2a2a2a";
          red =     "#bf1a08";
          green =   "#50a14f";
          yellow =  "#986801";
          blue =    "#4078f2";
          magenta = "#7807b5";
          cyan =    "#0184bc";
          white =   "#ffffff";
        };

      };

      window = {
        padding = {
          x = 16;
          y = 16;
        };
      };
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
    firefox
    thunderbird

    zoom-us
    element-desktop
    tdesktop
    discord

    gimp
    darktable

    jetbrains.idea-community

    nextcloud-client

    libreoffice-still

    xmobar
    betterlockscreen
    nitrogen
  ];
}
