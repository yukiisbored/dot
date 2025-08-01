{
  description = "Yuki's dotfiles";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, utils, nixpkgs, home-manager, nix-darwin, ... }:
    let
      inherit (home-manager.lib) homeManagerConfiguration;
      inherit (nixpkgs) lib;
      inherit (nix-darwin.lib) darwinSystem;

      mkConfiguration = system: config: 
        let
          inherit (lib.systems.elaborate { inherit system; }) isLinux isDarwin;

          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
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
          });
    in {
      packages = {
        aarch64-darwin.homeConfigurations.yuki = mkConfiguration "aarch64-darwin" ./home;
        x86_64-linux.homeConfigurations.yuki = mkConfiguration "x86_64-linux" ./home;
      };

      darwinConfigurations.leveilleur = darwinSystem {
        modules = [ 
          ./darwin.nix
        ];

        specialArgs = {
          inherit inputs;
          flake = self;
        };
      };

      darwinConfigurations.xanathaea = self.darwinConfigurations.leveilleur;
    };
}
