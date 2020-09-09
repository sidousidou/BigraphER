# Development image -- includes opam graphviz and emacs

FROM ocaml/opam2:ubuntu-20.04
LABEL maintainer="michele.sevegnani@glasgow.ac.uk"

# Install OS dependencies
RUN sudo apt-get -qy update && \
    sudo apt-get -qy --no-install-recommends install \
      m4 \
      zlib1g-dev \
      minisat \
      graphviz \
      emacs && \
    sudo apt-get clean && \
    sudo rm -rf /var/lib/apt/lists/*

# Install OCaml dependencies
RUN rm -rf opam-repository && \
    opam switch list -s | head -8 | xargs opam switch remove
RUN opam repo set-url --all-switches default https://opam.ocaml.org/ && \
    opam install -y dune dune-configurator jsonm menhir cmdliner && \
    opam clean -acr

# Build bigrapher
COPY --chown=opam . /home/opam/devel
WORKDIR /home/opam/devel
RUN eval $(opam env) && \
    dune build -j 4 --profile=release --always-show-command-line && \
    dune install && \
    dune clean
ENTRYPOINT ["bash", "-l"]

