{ stdenv, fetchFromGitHub, lib }:

stdenv.mkDerivation rec {
  name = "konfig";
  version = "0.2.6";

  src = fetchFromGitHub {
    owner = "corneliusweig";
    repo = "konfig";
    rev = "v${version}";
    sha256 = "sha256-qcYDiQJqXEWrGEwVdiB7922M8xT9mcbMdMBst5STOJk=";
  };

  buildPhase = "";

  installPhase = ''
    mkdir -p $out/bin
    cp ./konfig $out/bin
  '';

  meta = with lib; {
    description = "konfig helps to merge, split, or import kubeconfig files";
    homepage = "https://github.com/corenilusweig/konfig";
    license = licenses.asl20;
  };
}
