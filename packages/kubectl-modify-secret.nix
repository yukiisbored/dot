{ buildGoModule, fetchFromGitHub, lib }:

buildGoModule rec {
  pname = "kubectl-modify-secret";
  version = "0.0.42";

  src = fetchFromGitHub {
    owner = "rajatjindal";
    repo = "kubectl-modify-secret";
    rev = "v${version}";
    sha256 = "sha256-TLdQGoExX7dOf1jSHf7KNTeRFFWEeAEqkVkKy45xaYo=";
  };

  vendorSha256 = "sha256-RBpft/5P/EcTYzhBltYRZ8shOKcY5QGMSYXpqTcHF3w=";

  meta = with lib; {
    description = "Modify secrets without worrying about base64 encoding/decoding";
    homepage = "https://github.com/rajatjindal/kubectl-modify-secret";
    license = licenses.asl20;
  };
}
