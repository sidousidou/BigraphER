{ stdenv, fetchurl, buildOcaml, minisat, zlib }:

buildOcaml rec {
  name = "camlminisat-${version}";
  version = "0.5.3";

  src = fetchurl {
    url = "http://www.dcs.gla.ac.uk/~michele/arch/camlminisat-${version}.tar.gz";
    sha256 = "1h7b4srx35g68am5ra1ayq12lwzsdiwhjz725yjq5zzxhys7kx2s";
  };

  patches = [ ./cminisat.patch ];

  buildInputs = [ minisat zlib ];

  meta = with stdenv.lib; {
    description = "Ocaml bindings for minisat";
  };
}
