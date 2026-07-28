{ pkgs, flake, inputs, ... }: {
  imports = [
    inputs.home-manager.darwinModules.home-manager
  ];

  nixpkgs = {
    config.allowUnfree = true;
    hostPlatform = "aarch64-darwin";
  };

  nix = {
    enable = true;
    package = pkgs.nix;

    settings = {
      experimental-features = "nix-command flakes";
      trusted-users = ["root" "yuki"];
    };
  };

  system.primaryUser = "yuki";
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

  security.pam.services.sudo_local.touchIdAuth = true;

  ids.gids.nixbld = 350;

  system.configurationRevision = flake.rev or flake.dirtyRev or null;
  system.stateVersion = 4;
}
