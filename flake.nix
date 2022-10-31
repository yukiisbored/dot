{
  description = "Yuki's dotfiles";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, utils, nixpkgs, home-manager, ... }:
    let
      inherit (home-manager.lib) homeManagerConfiguration;

      system = "x86_64-linux";

      common.home = {
        username = "yuki";
        homeDirectory = "/home/yuki";
        stateVersion = "22.11";
      };

      pkgs = import nixpkgs {
        inherit system;

        config.allowUnfree = true;

        overlays = [
          inputs.emacs-overlay.overlay

          (self: super: {
            kubectl-modify-secret = self.callPackage ./packages/kubectl-modify-secret.nix {};
          })
        ];
      };
    in {
      homeConfigurations.core = homeManagerConfiguration {
        inherit pkgs;

        modules = [
          common
          ./modules/core.nix
        ];
      };

      homeConfigurations.generic = homeManagerConfiguration {
        inherit pkgs;

        modules = [
          common
          ./modules/core.nix
          ./modules/generic.nix
        ];
      };

      homeConfigurations.desktop = homeManagerConfiguration {
        inherit pkgs;

        modules = [
          common
          ./modules/core.nix
          ./modules/desktop.nix
        ];
      };
    };
}
