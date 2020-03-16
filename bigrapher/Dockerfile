FROM ocaml/opam2:ubuntu-19.04 AS build
LABEL maintainer="michele.sevegnani@glasgow.ac.uk"

# Install OS dependencies
RUN sudo apt-get -qy --no-install-recommends install \
                 m4 \
                 pkg-config \
                 graphviz \
                 zlib1g-dev \
                 minisat && \
    sudo apt-get clean

# Install OCaml dependencies
RUN opam repository add glasgow http://www.dcs.gla.ac.uk/~michele/dcs-opam-repository/ && \
    opam install -y dune bigraph menhir big_json cmdliner

# Build bigrapher
COPY --chown=opam . /home/opam/devel
WORKDIR /home/opam/devel
RUN eval $(opam env) && \
    dune build --profile=release

# Second stage image
FROM ubuntu:19.04
RUN apt-get update && \
    apt-get install -y \
      m4 \
      pkg-config \
      graphviz \
      zlib1g-dev \
      minisat && \
    rm -rf /var/lib/apt/lists/*
COPY --from=build /home/opam/devel/_build/install/default/bin/bigrapher /bin/bigrapher
ENTRYPOINT ["/bin/bigrapher"]
CMD ["--help"]
