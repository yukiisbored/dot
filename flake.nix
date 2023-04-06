{
  description = "Yuki's dotfiles";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
    gke-gcloud-auth-plugin = {
      url = "github:talzion12/gke-gcloud-auth-plugin-flake";
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

      pkgsCommon = {
        inherit system;
        config.allowUnfree = true;
      };

      pkgs = import nixpkgs (pkgsCommon // {
        overlays = [
          (self: super: {
            gke-gcloud-auth-plugin = inputs.gke-gcloud-auth-plugin.defaultPackage.${system};
            konfig = self.callPackage ./packages/konfig {};
          })
        ];
      });
    in {
      homeConfigurations.core = homeManagerConfiguration {
        inherit pkgs;

        modules = [
          inputs.nix-doom-emacs.hmModule
          common
          ./modules/core.nix
        ];
      };

      homeConfigurations.generic = homeManagerConfiguration {
        inherit pkgs;

        modules = [
          inputs.nix-doom-emacs.hmModule
          common
          ./modules/core.nix
          ./modules/generic.nix
        ];
      };
    };
}
