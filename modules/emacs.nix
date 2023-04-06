{ pkgs, ... }:
{
  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ../doom/.doom.d;
    emacsPackagesOverlay = self: super: {
      copilot = self.trivialBuild {
        pname = "copilot";
        ename = "copilot";
        version = "unstable-2023-04-06";
        buildInputs = [ self.s self.dash self.editorconfig self.jsonrpc ];
        src = pkgs.fetchFromGitHub {
          owner = "zerolfx";
          repo = "copilot.el";
          rev = "8256377a77b1268a9ea5d470c59a05d26ff5b432";
          sha256 = "sha256-KkKTN67IDfkmkYUo0M3aBbwI89LL9eAJqfyaTwye9Zc=";
        };

        postInstall = ''
           LISPDIR=$out/share/emacs/site-lisp
           cp -r dist $LISPDIR
        '';
      };
    };
  };

  home.packages = with pkgs; [
    wakatime

    (aspellWithDicts (ds: with ds; [ en en-computers en-science fr ]))
    languagetool

    fira
    julia-mono
    emacs-all-the-icons-fonts
    font-awesome_4
  ];
}
