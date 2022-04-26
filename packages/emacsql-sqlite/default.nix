{ stdenv, fetchFromGitHub, lib, sqlite }:

stdenv.mkDerivation rec {
  pname = "emacsql-sqlite";
  version = "3.0.0";

  src = fetchFromGitHub {
    owner = "skeeto";
    repo = "emacsql";
    rev = "${version}";
    sha256 = "sha256-L6CpSkJprgZRnzZrnLgxV4366fDt8VQWkViqGnJ/BLE=";
  };

  buildInputs = [ sqlite ];

  buildPhase = ''
    make binary
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp ./sqlite/emacsql-sqlite $out/bin/emacsql-sqlite
  '';

  meta = with lib; {
    description = "EmacSQL SQLite binary";
    homepage = "https://github.com/skeeto/emacsql";
    license = licenses.unlicense;
  };
}
