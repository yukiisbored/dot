{ pkgs, config, ... }:

{
  home.packages = with pkgs; [
    ungoogled-chromium
    thunderbird

    zoom-us
    element-desktop
    tdesktop

    gimp
    inkscape
    blender
    darktable
    aseprite-unfree # I bought Aseprite. Chill.

    jetbrains.idea-community
    jetbrains.rider

    nextcloud-client

    libreoffice-still

    lutris
    unityhub
  ];
}
