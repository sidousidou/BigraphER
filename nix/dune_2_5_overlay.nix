self: super: {
  ocamlPackages = super.ocamlPackages.overrideScope' (slf: spr: {
    dune_2 = spr.dune_2.overrideAttrs (o : rec {
      version = "2.5.0";
      src = self.fetchurl {
        url = "https://github.com/ocaml/dune/releases/download/${version}/dune-${version}.tbz";
        sha256 = "1nnpg0fvmp4vf5mk203xk83pkkm953pgip3yhs1x2g8pkcdndhcw";
        };
      });
  });
}
