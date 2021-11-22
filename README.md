# yuki/dot

[![NixOS 21.05](https://img.shields.io/badge/NixOS-v21.05-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

This repository contains my personal configuration which are managed
with mainly GNU stow.

a Nix flake is also provided to install and configure all of the
software that is utilized by Yuki.

## Using flake

See `flake.nix` for available homeConfigurations

If you want to replicate the entire desktop experience,

```console
$ nix run '.#desktop'
```

If you want to replicate the essentials,

```console
$ nix run '.#core'
```

## Software

| Function             | Software                                    |
|----------------------|---------------------------------------------|
| Operating systems    | NixOS, OpenBSD, WSL (Windows 10/Windows 11) |
| Display server       | X11 (Fuck Wayland shills)                   |
| Desktop Environment  | GNOME / XMonad                              |
| Font                 | Iosevka Term Slab / Iosevka Etoile          |
| Editor               | GNU Emacs                                   |
| Web browser          | Mozilla Firefox                             |
| Shell                | zsh                                         |
| File synchronisation | Nextcloud                                   |

## Mirrors

This repository is hosted on https://git.yukiisbo.red/yuki/dot

Mirrors are setup on both [GitHub.com][gh-mirror] and [GitLab.com][gl-mirror].

[gh-mirror]: https://github.com/yukiisbored/dot
[gl-mirror]: https://gitlab.com/yuki_is_bored/dot
