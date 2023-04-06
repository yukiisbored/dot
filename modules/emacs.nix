{ pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: [ epkgs.vterm ];
  };

  home.packages = with pkgs; [
    ripgrep
    fd

    wakatime

    (aspellWithDicts (ds: with ds; [ en en-computers en-science fr ]))
    languagetool

    fira
    julia-mono
    emacs-all-the-icons-fonts
    font-awesome_4
  ];
}
