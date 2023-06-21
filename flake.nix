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
      inherit (nixpkgs) lib;

      mkConfiguration = system: config: 
        let
          inherit (lib.systems.elaborate { inherit system; }) isLinux isDarwin;

          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;

            overlays = [
              (self: super: {
                inherit (inputs.devenv.packages.${system}) devenv;
                inherit (inputs.helix.packages.${system}) helix;
                konfig = self.callPackage ./packages/konfig {};
              })
            ] ++ lib.optionals isLinux [
              (self: super: {
                gke-gcloud-auth-plugin = inputs.gke-gcloud-auth-plugin.defaultPackage.${system};
              })
            ];
          };
        in
          homeManagerConfiguration ({
            inherit pkgs;

            modules = [
             ({
                home = {
                  homeDirectory = if isDarwin then "/Users/yuki" else "/home/yuki";
                  username = "yuki";
                  stateVersion = "22.11";
                };
              })

              config
            ];

            extraSpecialArgs = {
              inherit system isLinux isDarwin;
            };
          });
    in {
      homeConfigurations.core = mkConfiguration "x86_64-linux" ./configurations/core.nix;
      homeConfigurations.generic = mkConfiguration "x86_64-linux" ./configurations/generic.nix;
      homeConfigurations.mac = mkConfiguration "aarch64-darwin" ./configurations/core.nix;
    };
}
