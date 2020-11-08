{ mkDerivation, base, cereal, containers, cryptonite, stdenv }:
mkDerivation {
  pname = "dcc";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base cereal containers cryptonite ];
  description = "Maximally decentralized cryptocurrency";
  license = stdenv.lib.licenses.mit;
}
