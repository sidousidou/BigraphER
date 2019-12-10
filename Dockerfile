FROM ocaml/opam2:ubuntu-19.04-opam

MAINTAINER Michele Sevegnani <michele.sevegnani@glasgow.ac.uk>

# Install dependencies
RUN sudo apt-get -qy update &&\
    sudo apt-get -qy upgrade && \
    sudo apt-get -qy --no-install-recommends install \
                 m4 \
                 pkg-config \
                 graphviz \
                 zlib1g-dev \
                 minisat && \
    sudo apt-get clean

# Configure OPAM
RUN opam-sandbox-disable && \
    opam init -a && \
    opam repository add glasgow http://www.dcs.gla.ac.uk/~michele/dcs-opam-repository/ &&\
    opam install -y bigrapher

ENTRYPOINT [ "opam", "config", "exec", "--" ]
CMD bash
