FROM ocaml/opam2:ubuntu-19.04 AS build
LABEL maintainer="michele.sevegnani@glasgow.ac.uk"

# Install OS dependencies
RUN sudo apt-get update && \
    sudo apt-get -qy --no-install-recommends install \
      m4 \
      zlib1g-dev \
      minisat && \
    sudo apt-get clean

# Install OCaml dependencies
RUN opam repo set-url --all-switches default https://opam.ocaml.org/ && \
    opam update && \
    opam switch install 4.10.0+flambda && \
    opam install -y dune jsonm menhir cmdliner

# Build bigrapher
COPY --chown=opam . /home/opam/devel
WORKDIR /home/opam/devel
RUN eval $(opam env) && \
    dune build -j 4 --profile=release --always-show-command-line \
      --workspace dune-workspace.static; \
    exit 0
#    dune build -j 4 --profile=release --always-show-command-line
RUN ldd _build/install/default/bin/bigrapher ; \
    ldd _build/install/default/bin/big_match ; \
    exit 0

# Second stage image
FROM ubuntu:19.04
# RUN apt-get update && \
#     apt-get install -y \
#       graphviz \
#       zlib1g-dev \
#       minisat && \
#     rm -rf /var/lib/apt/lists/*
COPY --from=build /home/opam/devel/_build/install/default/bin/bigrapher /bin/bigrapher
COPY --from=build /home/opam/devel/_build/install/default/bin/big_match /bin/big_match
#COPY --from=build /home/opam/devel/_build/default/bigrapher/examples/*.big /root/
WORKDIR /root
