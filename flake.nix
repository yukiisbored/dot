{
  description = "Yuki's dotfiles";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gke-gcloud-auth-plugin = {
      url = "github:talzion12/gke-gcloud-auth-plugin-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devenv.url = "github:cachix/devenv";
    helix.url = "github:helix-editor/helix/23.05";
  };

  outputs = inputs @ { self, utils, nixpkgs, home-manager, ... }:
    let
      inherit (home-manager.lib) homeManagerConfiguration;

      system = "x86_64-linux";

      common = {
        home = {
          username = "yuki";
          homeDirectory = "/home/yuki";
          stateVersion = "22.11";
        };
      };

      pkgsCommon = {
        inherit system;
        config.allowUnfree = true;
      };

      pkgs = import nixpkgs (pkgsCommon // {
        overlays = [
          (self: super: {
            inherit (inputs.devenv.packages.${system}) devenv;
            inherit (inputs.helix.packages.${system}) helix;
            gke-gcloud-auth-plugin = inputs.gke-gcloud-auth-plugin.defaultPackage.${system};
            konfig = self.callPackage ./packages/konfig {};
          })
        ];
      });

      mkConfiguration = config: homeManagerConfiguration {
        inherit pkgs;

        modules = [
          common
          config
        ];
      };
    in {
      homeConfigurations.core = mkConfiguration ./configurations/core.nix;
      homeConfigurations.generic = mkConfiguration ./configurations/generic.nix;
    };
}
