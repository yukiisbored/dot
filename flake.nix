{
  description = "Yuki's dotfiles";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";

    home-manager = {
      url = "github:nix-community/home-manager/release-21.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    comma = {
      url = "github:Shopify/comma";
      flake = false;
    };

    nixpkgsFork.url = "github:yukiisbored/nixpkgs/yuki_is_bored/lutris-extra";
  };

  outputs = inputs @ { self, nixpkgs, home-manager, ... }:
    let
      overlays = [
        inputs.emacs-overlay.overlay
        (self: super: {
          comma = import "${inputs.comma}/default.nix" { pkgs = self; };
        })
        (self: super:
          let
            forkPkgs = import inputs.nixpkgsFork {
              system = "x86_64-linux";
              config.allowUnfree = true;
            };
          in
            {
              lutris = forkPkgs.lutris.override {
                extraLibraries = pkgs: with pkgs; [
                  opusfile
                ];
              };
            })
      ];
    in {
      homeConfigurations.core = home-manager.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        homeDirectory = "/home/yuki";
        username = "yuki";

        configuration = { pkgs, config, ... }:
          {
            xdg.configFile."nix/nix.conf".source = ./nix/nix.conf;
            nixpkgs.config.allowUnfree = true;
            nixpkgs.overlays = overlays;
            imports = [
              ./modules/core.nix
            ];
          };
      };

      homeConfigurations.desktop = home-manager.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        homeDirectory = "/home/yuki";
        username = "yuki";

        configuration = { pkgs, config, ... }:
          {
            xdg.configFile."nix/nix.conf".source = ./nix/nix.conf;
            nixpkgs.config.allowUnfree = true;
            nixpkgs.overlays = overlays;
            imports = [
              ./modules/core.nix
              ./modules/desktop.nix
            ];
          };
      };


      core = self.homeConfigurations.core.activationPackage;
      desktop = self.homeConfigurations.desktop.activationPackage;
    };
}
