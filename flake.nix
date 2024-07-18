{
  description = "Yuki's dotfiles";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    helix.url = "github:helix-editor/helix";
    zig.url = "github:mitchellh/zig-overlay";
    zls = {
      url = "github:zigtools/zls";
      inputs.zig-overlay.follows = "zig";
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

      overlays =
        [
         inputs.zig.overlays.default
         (self: super: {
           inherit (inputs.devenv.packages.${self.system}) devenv;
           inherit (inputs.helix.packages.${self.system}) helix;
           inherit (inputs.zls.packages.${self.system}) zls;
         })
        ];

      mkConfiguration = system: config: 
        let
          inherit (lib.systems.elaborate { inherit system; }) isLinux isDarwin;

          pkgs = import nixpkgs {
            inherit system overlays;
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

            extraSpecialArgs = {
              inherit system isLinux isDarwin;
            };
          });
    in {
      packages = {
        aarch64-darwin.homeConfigurations.yuki = mkConfiguration "aarch64-darwin" ./home;
        x86_64-linux.homeConfigurations.yuki = mkConfiguration "x86_64-linux" ./home;
      };

      darwinConfigurations.leveilleur = darwinSystem {
        modules = [ 
          { nixpkgs.overlays = overlays; }
          ./darwin.nix
        ];

        specialArgs = {
          inherit inputs;
          flake = self;
        };
      };
    };
}
