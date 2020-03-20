with import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.03-beta.tar.gz") {};

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
