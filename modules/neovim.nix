{ pkgs, lib, ... }:
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
  };

  home.packages = with pkgs; [
    git
    lazygit

    (ripgrep.override { withPCRE2 = true; })
    fd
  ];

  home.activation = {
    installLazyVim = lib.hm.dag.entryAfter ["writeBoundary"] ''
      export XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-$HOME/.config}

      if [ ! -d "$XDG_CONFIG_HOME/nvim" ]; then
        $DRY_RUN_CMD ${pkgs.git}/bin/git clone $VERBOSE_ARG https://github.com/yukiisbored/lazyvim "$XDG_CONFIG_HOME/nvim"
      fi
    '';
  };
}
