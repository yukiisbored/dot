{
  description = "Yuki's dotfiles";

  inputs = {
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
    nixpkgsFork.url = "github:yukiisbored/nixpkgs/yuki_is_bored/lutris-extra";
    nixpkgsUnstable.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = inputs @ { self, nixpkgs, home-manager, ... }:
    let
      supportedSystems = nixpkgs.lib.platforms.unix;

      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      pkgsFor = forAllSystems (system:
        let
          config = {
            allowUnfree = true;
          };

          forkPkgs = import inputs.nixpkgsFork {
            inherit system;
            inherit config;
          };

          unstablePkgs = import inputs.nixpkgsUnstable {
            inherit system;
            inherit config;
          };
        in
          import nixpkgs {
            inherit system;
            inherit config;

            overlays = [
              inputs.emacs-overlay.overlay

              (self: super: {
                comma = import "${inputs.comma}/default.nix" { pkgs = self; };

                lutris = forkPkgs.lutris.override {
                  extraLibraries = pkgs: with pkgs; [
                    opusfile
                  ];
                };

                rnix-lsp = inputs.rnix-lsp.defaultPackage.${system};

                python38Packages.python-lsp-server = unstablePkgs.python38Packages.python-lsp-server;
              })
            ];
          });
    in {
      homeConfigurations.core = home-manager.lib.homeManagerConfiguration rec {
        system = "x86_64-linux";
        pkgs = pkgsFor.${system};

        homeDirectory = "/home/yuki";
        username = "yuki";

        configuration = { pkgs, config, ... }: {
          imports = [
            ./modules/core.nix
          ];
        };
      };

      homeConfigurations.desktop = home-manager.lib.homeManagerConfiguration rec {
        system = "x86_64-linux";
        pkgs = pkgsFor.${system};

        homeDirectory = "/home/yuki";
        username = "yuki";

        configuration = { pkgs, config, ... }: {
          imports = [
            ./modules/core.nix
            ./modules/desktop.nix
          ];
        };
      };


      core = self.homeConfigurations.core.activationPackage;
      desktop = self.homeConfigurations.desktop.activationPackage;
    };
}
