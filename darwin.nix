{ pkgs, flake, inputs, ... }: {
  imports = [
    inputs.home-manager.darwinModules.home-manager
  ]; 

  nixpkgs = {
    config.allowUnfree = true;
    hostPlatform = "aarch64-darwin";
  };

  services.nix-daemon.enable = true;

  nix = {
    package = pkgs.nix;

    settings = {
      experimental-features = "nix-command flakes";
      trusted-users = ["root" "yuki"];
    };
  };

  programs.zsh.enable = true;

  users.users.yuki = {
    name = "yuki";
    home = "/Users/yuki";
  };

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;

    extraSpecialArgs = {
      system = "aarch64-darwin";
      isLinux = false;
      isDarwin = true;
    };

    users.yuki = { ... }: {
      imports = [
        ./home
      ];

      home.stateVersion = "24.05";
    };
  };

  services.tailscale.enable = true;
  services.trezord.enable = true;

  security.pam.enableSudoTouchIdAuth = true;

  system = {
    activationScripts.postUserActivation.text = ''
      /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
    '';

    defaults = {
      menuExtraClock.Show24Hour = true;

      dock = {
        autohide = true;
        show-recents = false;
        orientation = "bottom";
      };

      finder = {
        _FXShowPosixPathInTitle = false;
        AppleShowAllExtensions = true;
        FXEnableExtensionChangeWarning = false;
        QuitMenuItem = true;
        ShowPathbar = true;
        ShowStatusBar = false;
        CreateDesktop = false;
        FXPreferredViewStyle = "clmv";
      };

      loginwindow = {
        GuestEnabled = false;
      };

      NSGlobalDomain = {
        AppleInterfaceStyle = null;
        # Control modals with Keyboard
        AppleKeyboardUIMode = 3;
        # Disable accent input on hold
        ApplePressAndHoldEnabled = false;

        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticDashSubstitutionEnabled = false;
        NSAutomaticPeriodSubstitutionEnabled = false;
        NSAutomaticQuoteSubstitutionEnabled = false;
        NSAutomaticSpellingCorrectionEnabled = false;
        NSNavPanelExpandedStateForSaveMode = true;
        NSNavPanelExpandedStateForSaveMode2 = true;
      };

      CustomUserPreferences = {
        NSGlobalDomain = {
          WebKitDeveloperExtras = true;
        };

        "com.apple.finder" = {
          # Search current folder first
          FXDefaultSearchScope = "SCcf";
        };

        "com.apple.desktopservices" = {
          # Avoid creating .DS_Store files on network or USB volumes
          DSDontWriteNetworkStores = true;
          DSDontWriteUSBStores = true;
        };
      };

    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToEscape  = true;
    };
  };

  system.configurationRevision = flake.rev or flake.dirtyRev or null;
  system.stateVersion = 4;
}
