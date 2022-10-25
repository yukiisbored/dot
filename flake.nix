{
  description = "Yuki's dotfiles";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, utils, home-manager, ... }:
    with utils.lib; eachDefaultSystem (system:
      let
        config = {
          allowUnfree = true;
        };

        overlays = [
          inputs.emacs-overlay.overlay

          (self: super: {
            kubectl-modify-secret = self.callPackage ./packages/kubectl-modify-secret.nix {};
            emacsql-sqlite = self.callPackage ./packages/emacsql-sqlite {};
          })
        ];

        pkgs = import inputs.nixpkgs { inherit system config overlays; };

        homeConfig = imports: ({ ... }: { inherit imports; });
        mkActivationPackage = configuration: (home-manager.lib.homeManagerConfiguration {
          inherit system pkgs configuration;
          homeDirectory = "/home/yuki";
          username = "yuki";
        }).activationPackage;
      in
        rec {
          homeConfigurations = {
            core = homeConfig [ ./modules/core.nix ];
            generic = homeConfig [ ./modules/core.nix ./modules/generic.nix ];
            desktop = homeConfig [ ./modules/core.nix ./modules/desktop.nix ];
          };

          packages = builtins.mapAttrs (_: x: mkActivationPackage x) homeConfigurations;
          apps = builtins.mapAttrs (_: x: mkApp { drv = x; }) packages;
        }
    );
}
