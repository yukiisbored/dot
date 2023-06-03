{ pkgs, ... }:
{
  programs.git = {
    enable = true;
    userName = "Yuki Langley";
    userEmail = "hi@yukiisbo.red";

    extraConfig = {
      init.defaultBranch = "main";
    };

    lfs.enable = true;
  };

  home.packages = with pkgs; [
    gitui
    git-crypt
  ];
}
