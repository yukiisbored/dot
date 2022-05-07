# yuki/dot

[![NixOS 21.11](https://img.shields.io/badge/NixOS-v21.11-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

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

| Function             | Software                                            |
|----------------------|-----------------------------------------------------|
| Operating systems    | Debian, NixOS, OpenBSD, WSL (Windows 10/Windows 11) |
| Display server       | X11 (Still, fuck Wayland shills)                    |
| Desktop Environment  | KDE / XMonad                                        |
| Font                 | Iosevka Term Slab / Iosevka Etoile                  |
| Editor               | GNU Emacs                                           |
| Web browser          | Vivaldi (everyone expects Chromium now, I give up)  |
| Shell                | zsh                                                 |
| File synchronisation | Seafile                                             |

## Mirrors

This repository is hosted on https://git.yukiisbo.red/yuki/dot

Mirrors are setup on both [GitHub.com][gh-mirror] and [GitLab.com][gl-mirror].

[gh-mirror]: https://github.com/yukiisbored/dot
[gl-mirror]: https://gitlab.com/yuki_is_bored/dot
