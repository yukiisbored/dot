{ pkgs, config, ... }:

{
  home.packages = with pkgs; [
    (vivaldi.override {
      proprietaryCodecs = true;
      enableWidevine = true;
    })

    element-desktop
    tdesktop
    discord
    gajim

    resilio-sync

    gimp
    inkscape
    libreoffice-still

    gnome.gnome-tweaks
    appimage-run
  ] ++ (with pkgs.gnomeExtensions; [
    dash-to-panel
    openweather
    appindicator
    x11-gestures
  ]);
}
