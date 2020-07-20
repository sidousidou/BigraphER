let
  pkgs = import <nixpkgs> {};
in

let
  bigrapher = pkgs.callPackage ../static.nix {};
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
