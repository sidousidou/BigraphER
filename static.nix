{} :

let pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz")
  {
    overlays = [
      (import ./nix/dune_2_5_overlay.nix)
      (import ./nix/minisat_static_overlay.nix)
    ];
  };
in
with pkgs;

let
  ocaml_static = pkgsStatic.ocaml.overrideDerivation (o: {
    preConfigure = ''
            configureFlagsArray+=("CC=$CC" "AS=$AS" "PARTIALLD=$LD -r")
          '';
    configureFlags = (pkgs.lib.remove "--no-shared-libs" o.configureFlags) ++ [
      "--disable-shared"
      "--host ${o.stdenv.hostPlatform.config}"
      "--target ${o.stdenv.targetPlatform.config}"
    ];
  });
in

pkgsStatic.stdenv.mkDerivation rec {
  name = "bigrapher-${version}";
  version = "1.9.3";

  patches = [ ./nix/minicard_static_build.patch ./nix/minicard_static.patch ];

  src = lib.cleanSource ./.;

  buildInputs = with ocamlPackages; [ pkgsStatic.minisat pkgsStatic.zlib jsonm cmdliner dune-configurator mtime ];
  nativeBuildInputs = with ocamlPackages; [ dune_2 ocaml_static findlib menhir ];

  buildPhase = ''
      dune build @install --profile=release
    '';

  installPhase = ''
      dune install --profile=release --prefix=$out
    '';

}
