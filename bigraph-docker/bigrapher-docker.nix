with import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz") {
  overlays = [ (import ../nix/dune_2_5_overlay.nix) (import ../nix/minisat_overlay.nix) ];
};

let
  bigrapher = pkgs.callPackage ../default.nix {};
in
pkgs.dockerTools.buildImage {
  name = "bigrapher";
  tag = "latest";
  contents = [ bigrapher pkgs.graphviz ];
  config = {
    EntryPoint = "bigrapher";
    WorkingDir = "/data";
  };
}
