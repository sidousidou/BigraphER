{ stdenv, fetchgit , zlib }:

stdenv.mkDerivation rec {
  name = "minisat-${version}";
  version = "2.2.0";

  src = fetchgit {
    url = "https://github.com/niklasso/minisat.git";
    rev = "37dc6c67e2af26379d88ce349eb9c4c6160e8543";
    sha256 = "091hf3qkm197s5r7xcr3m07xsdwyz2rqk1hc9kj0hn13imz09irq";
  };

  buildInputs = [ zlib ];

  preConfigure = ''make config prefix=$out'';

  patches = [ ./minisat.patch ];

  makeFlags = [ "lsh" ];
  installPhase = ''
    make install
  '';

  meta = with stdenv.lib; {
    description = "Compact and readable SAT solver";
    platforms = platforms.unix;
    license = licenses.mit;
    homepage = http://minisat.se/;
  };
}
