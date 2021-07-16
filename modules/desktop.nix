{ pkgs, config, ... }:

{
  home.packages = with pkgs; [
    ungoogled-chromium
    thunderbird

    element-desktop
    tdesktop

    gimp
    inkscape
    blender
    darktable

    jetbrains.idea-community

    nextcloud-client

    libreoffice-still
  ];
}
