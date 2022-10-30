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

    remmina
    moonlight-qt
    synergy

    qpwgraph
    easyeffects
  ] ++ (with pkgs.gnomeExtensions; [
    dash-to-panel
    openweather
    appindicator
    x11-gestures
  ]);
}
