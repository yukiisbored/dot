# yuki/dot

[![NixOS 22.05](https://img.shields.io/badge/NixOS-v22.05-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

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

| Function          | Software             |
|-------------------|----------------------|
| Operating systems | Windows 10, openSUSE |
| Font              | Iosevka Term Slab    |
| Editor            | GNU Emacs            |
| Web browser       | Vivaldi              |
| Shell             | zsh                  |

I don't use NixOS anymore because:

1. I either find NixOS modules to be getting in the way for system configuration
   and I rarely find myself to have the mental energy to hack them.

2. I need to run non-free software which relies on FHS and other traditional
   Linux-isms and I rarely find myself to have the mental energy to hack them.

It's a great operating system for a lot of people, especially those who are
first venturing in their journey, but sadly, I've been living within the arcane 
horrors of UNIX system administration and old habits die hard.

Good luck fellow travelers o/

## Mirrors

This repository is hosted on https://git.yukiisbo.red/yuki/dot

Mirrors are setup on both [GitHub.com][gh-mirror] and [GitLab.com][gl-mirror].

[gh-mirror]: https://github.com/yukiisbored/dot
[gl-mirror]: https://gitlab.com/yuki_is_bored/dot
