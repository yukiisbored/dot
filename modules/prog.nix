{ pkgs, lib, isLinux, ... }:
{
  home.packages = with pkgs; [
    # Python
    pipenv
    poetry

    # JavaScript
    nodejs-18_x
    deno

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

    # Java
    temurin-bin

    reviewdog
  ] ++ lib.optionals isLinux (with pkgs; [
    valgrind
  ]);
}
