{ pkgs, lib, isLinux, ... }:
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
    zls

    # C, C++, and friends!
    cmake

    # Rust
    rustc
    cargo
    rust-analyzer

    # Benchmark
    hyperfine
  ] ++ lib.optionals isLinux (with pkgs; [
    valgrind
  ]);
}
