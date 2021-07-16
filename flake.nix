{
  description = "Yuki's dotfiles";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";

    home-manager = {
      url = "github:nix-community/home-manager/release-21.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = inputs @ { self, nixpkgs, home-manager, ... }:
    let
      overlays = [
        inputs.emacs-overlay.overlay
      ];
    in {
      homeConfigurations.linux = home-manager.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        homeDirectory = "/home/yuki";
        username = "yuki";

        configuration = { pkgs, config, ... }:
          {
            xdg.configFile."nix/nix.conf".source = ./nix/nix.conf;
            nixpkgs.config.allowUnfree = true;
            nixpkgs.overlays = overlays;
            imports = [
              ./nix/home.nix
            ];
          };
      };

      linux = self.homeConfigurations.linux.activationPackage;
    };
}
