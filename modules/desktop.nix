{ pkgs, config, ... }:

{
  services.syncthing.enable = true;

  home.packages = with pkgs; [
    vivaldi
    vivaldi-widevine
    vivaldi-ffmpeg-codecs

    thunderbird

    zoom-us
    element-desktop
    tdesktop
    discord

    gimp
    darktable

    jetbrains.idea-community

    libreoffice-still

    appimage-run
  ];
}
