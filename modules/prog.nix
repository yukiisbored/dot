{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # Python
    pipenv
    poetry
    nodePackages.pyright

    # JavaScript
    nodejs-18_x
    deno
    nodePackages.typescript-language-server

    # iOS
    cocoapods

    # Zig
    zigpkgs.master

    # C, C++, and friends!
    cmake
  ];
}
