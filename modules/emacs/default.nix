{ pkgs, lib, ... }:
{
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.vterm
    ];
  };

  home.packages = with pkgs; [
    git
    (ripgrep.override { withPCRE2 = true; })
    gnutls

    fd
    imagemagick
    zstd

    (aspellWithDicts (ds: with ds; [ en en-computers en-science fr ]))
    languagetool

    editorconfig-core-c

    wakatime

    fantasque-sans-mono
    emacs-all-the-icons-fonts
  ];

  home.activation = {
    installDoomEmacs = lib.hm.dag.entryAfter ["writeBoundary"] ''
      if [ ! -d "$XDG_CONFIG_HOME/emacs" ]; then
        $DRY_RUN_CMD ${pkgs.git}/bin/git clone $VERBOSE_ARG --depth=1 --single-branch https://github.com/doomemacs/doomemacs "$XDG_CONFIG_HOME/emacs"
      fi

      if [ ! -d "$XDG_CONFIG_HOME/doom" ]; then
        $DRY_RUN_CMD ${pkgs.git}/bin/git clone $VERBOSE_ARG https://git.yukiisbo.red/yuki/doom "$XDG_CONFIG_HOME/doom"
      fi
    '';
  };
}
