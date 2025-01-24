{ pkgs, config, ... }:
{
  programs.zsh = {
    enable = true;

    autosuggestion.enable = true;

    enableCompletion = true;
    enableVteIntegration = true;

    autocd = true;
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
      {
          name = "vi-mode";
          src = pkgs.zsh-vi-mode;
          file = "share/zsh-vi-mode/zsh-vi-mode.plugin.zsh";
      }
    ];

    initExtra = builtins.readFile ./zshrc;
    envExtra = builtins.readFile ./zshenv;
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.eza = {
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
        vimcmd_symbol = "[%](green)";
      };

      status.symbol = "[x](bold red) ";

      git_branch.symbol = "git ";
      git_commit.tag_symbol = " tag ";
      git_status = {
        ahead = ">";
        behind = "<";
        diverged = "<>";
        renamed = "r";
        deleted = "x";
      };

      fossil_branch.symbol = "fossil ";
      hg_branch.symbol = "hg ";

      directory.read_only = " ro";

      docker_context.symbol = "docker ";
      gcloud.symbol = "gcp ";

      memory_usage.symbol = "memory ";

      nix_shell.symbol = "nix ";
      guix_shell.symbol = "guix ";

      os.symbols = {
        AIX = "aix ";
        Alpaquita = "alq ";
        AlmaLinux = "alma ";
        Alpine = "alp ";
        Amazon = "amz ";
        Android = "andr ";
        Arch = "rch ";
        Artix = "atx ";
        CentOS = "cent ";
        Debian = "deb ";
        DragonFly = "dfbsd ";
        Emscripten = "emsc ";
        EndeavourOS = "ndev ";
        Fedora = "fed ";
        FreeBSD = "fbsd ";
        Garuda = "garu ";
        Gentoo = "gent ";
        HardenedBSD = "hbsd ";
        Illumos = "lum ";
        Kali = "kali ";
        Linux = "lnx ";
        Mabox = "mbox ";
        Macos = "mac ";
        Manjaro = "mjo ";
        Mariner = "mrn ";
        MidnightBSD = "mid ";
        Mint = "mint ";
        NetBSD = "nbsd ";
        NixOS = "nix ";
        OpenBSD = "obsd ";
        OpenCloudOS = "ocos ";
        openEuler = "oeul ";
        openSUSE = "osuse ";
        OracleLinux = "orac ";
        Pop = "pop ";
        Raspbian = "rasp ";
        Redhat = "rhl ";
        RedHatEnterprise = "rhel ";
        RockyLinux = "rky ";
        Redox = "redox ";
        Solus = "sol ";
        SUSE = "suse ";
        Ubuntu = "ubnt ";
        Ultramarine = "ultm ";
        Unknown = "unk ";
        Void = "void ";
        Windows = "win ";
      };

      package.symbol = "pkg ";

      aws.symbol = "aws ";
      azure.symbol = "az ";
      bun.symbol = "bun ";
      c.symbol = "C ";
      cobol.symbol = "cobol ";
      conda.symbol = "conda ";
      crystal.symbol = "cr ";
      cmake.symbol = "cmake ";
      daml.symbol = "daml ";
      dart.symbol = "dart ";
      deno.symbol = "deno ";
      dotnet.symbol = ".NET ";
      elixir.symbol = "exs ";
      elm.symbol = "elm ";
      fennel.symbol = "fnl ";
      gleam.symbol = "gleam ";
      golang.symbol = "go ";
      gradle.symbol = "gradle ";
      java.symbol = "java ";
      julia.symbol = "jl ";
      kotlin.symbol = "kt ";
      lua.symbol = "lua ";
      nodejs.symbol = "nodejs ";
      meson.symbol = "meson ";
      nats.symbol = "nats ";
      nim.symbol = "nim ";
      ocaml.symbol = "ml ";
      opa.symbol = "opa ";
      perl.symbol = "pl ";
      php.symbol = "php ";
      pijul_channel.symbol = "pijul ";
      pulumi.symbol = "pulumi ";
      purescript.symbol = "purs ";
      python.symbol = "py ";
      quarto.symbol = "quarto ";
      raku.symbol = "raku ";
      ruby.symbol = "rb ";
      rust.symbol = "rs ";
      scala.symbol = "scala ";
      spack.symbol = "spack ";
      solidity.symbol = "solidity ";
      sudo.symbol = "sudo ";
      swift.symbol = "swift ";
      typst.symbol = "typst ";
      terraform.symbol = "terraform ";
      zig.symbol = "zig ";
    };
  };

  programs.tmux = {
    enable = true;
    escapeTime = 0;
    terminal = "tmux-256color";
    extraConfig = ''
      set -as terminal-features ",xterm-256color:RGB"
    '';
  };
}
