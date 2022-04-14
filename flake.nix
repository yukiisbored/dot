{
  description = "Yuki's dotfiles";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    comma = {
      url = "github:nix-community/comma";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
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
            comma = inputs.comma.defaultPackage.${system};
            kubectl-modify-secret = self.callPackage ./packages/kubectl-modify-secret.nix {};
            inherit (self.callPackage ./packages/localtunnel {}) localtunnel;
          })
        ];

        pkgs = import inputs.nixpkgs {
          inherit system;
          inherit config;
          inherit overlays;
        };

        homeConfig = imports: ({ ... }: { inherit imports; });
        mkActivationPackage = configuration: (home-manager.lib.homeManagerConfiguration {
          inherit system;
          inherit pkgs;
          inherit configuration;

          homeDirectory = "/home/yuki";
          username = "yuki";
        }).activationPackage;
      in
        rec {
          inherit overlays;

          homeConfigurations = {
            core = homeConfig [ ./modules/core.nix ];
            desktop = homeConfig [ ./modules/core.nix ./modules/desktop.nix ];
            generic = homeConfig [ ./modules/core.nix ./modules/generic.nix ];
          };

          packages = builtins.mapAttrs (_: x: mkActivationPackage x) homeConfigurations;
          defaultPackage = packages.generic;

          apps = builtins.mapAttrs (_: x: mkApp { drv = x; }) packages;
          defaultApp = apps.generic;
        }
    );
}
