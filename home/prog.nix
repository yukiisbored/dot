{ pkgs, lib, isLinux, isDarwin, ... }:
{
  home.packages = with pkgs; [
    # Python
    pipenv
    poetry

    # JavaScript
    fnm

    # Zig
    zls

    # C, C++, and friends!
    cmake

    # Rust
    rustup

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
