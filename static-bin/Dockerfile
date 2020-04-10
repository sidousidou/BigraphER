# Libc build image
FROM ocaml/opam2:ubuntu-19.04 AS build-libc

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
RUN ldd _build/install/default/bin/bigrapher ; \
    ldd _build/install/default/bin/big_match ; \
    exit 0
# ========================================================

# Musl build image
FROM ocaml/opam2:alpine-3.10 AS build-musl

# Install OS dependencies
RUN sudo apk update && \
    sudo apk add --no-cache git m4 zlib-dev

# Install MiniSAT
COPY --chown=opam . /home/opam/devel
WORKDIR /home/opam/devel/
RUN git clone -q https://github.com/niklasso/minisat ext_lib
WORKDIR ext_lib
RUN git checkout -q 37dc6c67e2af26379d88ce349eb9c4c6160e8543 && \
    patch -p1 < ../static-bin/fpu_control.patch && \
    make config prefix=/usr && \
    make && \
    sudo make install

# Install OCaml dependencies
RUN opam repo set-url --all-switches default https://opam.ocaml.org/ && \
    opam update && \
    opam switch install 4.10.0+musl+static+flambda && \
    opam install -y dune jsonm menhir cmdliner

# Build bigrapher
WORKDIR /home/opam/devel
RUN eval $(opam env) && \
    dune build -j 4 --profile=release --always-show-command-line \
      --workspace dune-workspace.static; \
    exit 0
RUN ldd _build/install/default/bin/bigrapher ; \
    ldd _build/install/default/bin/big_match ; \
    exit 0
# ========================================================

# Second stage image
FROM ubuntu:19.04
COPY --from=build-libc /home/opam/devel/_build/install/default/bin/bigrapher /bin/bigrapher
COPY --from=build-libc /home/opam/devel/_build/install/default/bin/big_match /bin/big_match
COPY --from=build-musl /home/opam/devel/_build/install/default/bin/bigrapher /bin/musl/bigrapher
COPY --from=build-musl /home/opam/devel/_build/install/default/bin/big_match /bin/musl/big_match
WORKDIR /root
