{ pkgs, lib, isDarwin, ... }:
{
  home.packages = with pkgs; [
    # Python
    poetry
    uv

    # JavaScript
    fnm

    # Benchmark
    hyperfine
  ] ++ lib.optionals isDarwin (with pkgs; [
    cocoapods
  ]);
}
