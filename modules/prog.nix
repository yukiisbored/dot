{ pkgs, lib, isLinux, isDarwin, ... }:
{
  home.packages = with pkgs; [
    # Python
    pipenv
    poetry

    # JavaScript
    bun
    nodejs-18_x
    deno

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
  ]) ++ lib.optionals isDarwin (with pkgs; [
    cocoapods
  ]);
}
