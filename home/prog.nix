{ pkgs, lib, isDarwin, ... }:
{
  home.packages = with pkgs; [
    # Python
    pipenv
    poetry

    # JavaScript
    fnm

    # Benchmark
    hyperfine
  ] ++ lib.optionals isDarwin (with pkgs; [
    cocoapods
  ]);
}
