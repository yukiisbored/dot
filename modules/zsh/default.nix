{ pkgs, config, ... }:
{
  programs.zsh = {
    enable = true;

    enableAutosuggestions = true;
    enableCompletion = true;
    enableVteIntegration = true;
    autocd = true;
    defaultKeymap = "emacs";
    history = {
      extended = true;
      path = "${config.home.homeDirectory}/.histfile";
    };

    plugins = [
      {
        name = "fast-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zdharma-continuum";
          repo = "fast-syntax-highlighting";
          rev = "770bcd986620d6172097dc161178210855808ee0";
          sha256 = "T4k0pbT7aqLrIRIi2EM15LXCnpRFHzFilAYfRG6kbeY=";
        };
      }
      {
        name = "zsh-fzf-history-search";
        src = pkgs.fetchFromGitHub {
          owner = "joshskidmore";
          repo = "zsh-fzf-history-search";
          rev = "f2432b240c40cff889aab3f10272b8466fb3d9ab";
          sha256 = "+reiUhVneaz2u170cltpYtYaHhu9wvaZuhf8TdJIrGs=";
        };
      }
      {
        name = "fzf-tab";
        src = pkgs.zsh-fzf-tab + /share/fzf-tab;
      }
    ];

    initExtra = builtins.readFile ./zshrc;
    envExtra = builtins.readFile ./zshenv;
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.exa = {
    enable = true;
    enableAliases = true;
  };

  programs.scmpuff = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      format = "$directory$character";
      right_format = "$all";
      add_newline = false;

      directory = {
        truncate_to_repo = false;
        truncation_symbol = "../";
      };

      character = {
        success_symbol = "%";
        error_symbol = "[%](red)";
      };

      gcloud.disabled = true;
    };
  };

  programs.tmux = {
    enable = true;
    escapeTime = 0;
  };
}
