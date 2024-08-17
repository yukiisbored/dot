{ pkgs, lib, ... }:

{
  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
  };

  programs.vscode.enable = true;

  home.packages = with pkgs; [
    jetbrains.webstorm
    jetbrains.pycharm-professional
    jetbrains.phpstorm
    jetbrains.idea-ultimate
    jetbrains.datagrip
    jetbrains.rust-rover
  ];
}
