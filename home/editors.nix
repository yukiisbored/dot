{ pkgs, ... }:

{
  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    extraPackages = with pkgs; [
      git
      lazygit

      findutils
      fzf
      ripgrep
      fd

      lua-language-server
      stylua

      nodejs_22
    ];
  };
}
