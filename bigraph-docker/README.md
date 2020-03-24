Building a Docker Version of BigraphER
==========================================

To build a docker image we again use NixOS. You can build the image using:

``` bash
nix-build bigrapher-docker
```

Which will create a `result` file in the current directory.

This can be loaded with:

```bash
docker load <result
```

And the container can be run, e.g.

```bash
docker run bigrapher full --help
```

Wrapping the CLI
================

To make it easier to use bigraphER as a CLI tool the script `bigrapher` can be
used. This counts the current directory as a new volume allowing files to move
to the docker container.

Note: As only the current directory is mounted you cannot do, for example:

``` bash
./bigrapher full ../bigrapher/examples/actors.big
```

But you can do

cat ../bigrapher/examples/actors.big | ./bigrapher full
