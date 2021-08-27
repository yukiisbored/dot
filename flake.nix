{
  description = "Yuki's dotfiles";

  inputs = {
    utils.url = "github:numtide/flake-utils";

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
    rnix-lsp.url = "github:nix-community/rnix-lsp";
    nixpkgsUnstable.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = inputs @ { self, utils, home-manager, ... }:
    with utils.lib; eachDefaultSystem (system:
      let
        config = {
          allowUnfree = true;
        };

        unstablePkgs = import inputs.nixpkgsUnstable {
          inherit system;
          inherit config;
        };

        pkgs = import inputs.nixpkgs {
          inherit system;
          inherit config;

          overlays = [
            inputs.emacs-overlay.overlay

            (self: super: {
              comma = import "${inputs.comma}/default.nix" { pkgs = self; };

              lutris = unstablePkgs.lutris.override {
                extraLibraries = pkgs: with pkgs; [
                  opusfile
                ];
              };

              rnix-lsp = inputs.rnix-lsp.defaultPackage.${system};

              python38Packages.python-lsp-server = unstablePkgs.python38Packages.python-lsp-server;
            })
          ];
        };

        homeConfig = imports: (home-manager.lib.homeManagerConfiguration {
          inherit system;
          inherit pkgs;

          homeDirectory = "/home/yuki";
          username = "yuki";

          configuration = { ... }: {
            inherit imports;
          };
        }).activationPackage;
      in
        rec {
          packages = {
            core = homeConfig [ ./modules/core.nix ];
            desktop = homeConfig [ ./modules/core.nix ./modules/desktop.nix ];
          };

          defaultPackage = packages.desktop;

          apps = {
            core = mkApp { drv = packages.core; };
            desktop = mkApp { drv = packages.desktop; };
          };

          defaultApp = apps.desktop;
        }
    );
}
