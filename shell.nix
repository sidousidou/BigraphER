let

minisat_overlay = self: super: {
    minisat = self.callPackage ./nix/minisat.nix {};
};

dune_2_5_overlay = self: super: {
  ocamlPackages = super.ocamlPackages.overrideScope' (slf: spr: {
    dune_2 = spr.dune_2.overrideAttrs (o : rec {
      version = "2.5.0";
      src = self.fetchurl {
        url = "https://github.com/ocaml/dune/releases/download/${version}/dune-${version}.tbz";
        sha256 = "1nnpg0fvmp4vf5mk203xk83pkkm953pgip3yhs1x2g8pkcdndhcw";
        };
      });
  });
};
in

let
pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz") {
    overlays = [ dune_2_5_overlay minisat_overlay ];
};

in with pkgs;
mkShell {
  buildInputs = [
    zlib
    minisat

    ocaml
    ocamlformat
    ocamlPackages.dune_2
    ocamlPackages.findlib
    ocamlPackages.utop
    ocamlPackages.menhir
    ocamlPackages.jsonm
    ocamlPackages.cmdliner
    ocamlPackages.odoc
    ocamlPackages.dune-configurator
  ];
}
