{ pkgs, config, ... }:

{
  home.packages = with pkgs; [
    (vivaldi.override {
      proprietaryCodecs = true;
    })

    element-desktop
    tdesktop
    discord
    gajim

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

    heroic
    prismlauncher
  ] ++ (with pkgs.gnomeExtensions; [
    dash-to-panel
    openweather
    appindicator
    x11-gestures
  ]);

  systemd.user.services.rslsync = {
    Unit = {
      Description = "Resilio Sync per-user service";
    };

    Service = {
      Type = "simple";
      ExecStart = "${pkgs.resilio-sync}/bin/rslsync --nodaemon";
      Restart = "on-abort";
    };

    Install = {
      WantedBy = [ "default.target" ];
    };
  };
}
