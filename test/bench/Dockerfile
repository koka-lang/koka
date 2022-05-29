# Create a docker image for running the benchmarks.
# Build:
# > docker build -t <mytag> .
#
# To Run:
# > docker run -it <mytag>
# or
# > docker run -v <local-dir> -it <mytag>

FROM ubuntu:20.04
RUN apt-get update
RUN apt-get install -y --no-install-recommends ca-certificates
RUN apt-get install -y --no-install-recommends cmake make
RUN apt-get install -y --no-install-recommends git
RUN apt-get install -y --no-install-recommends gcc libc-dev build-essential tar
RUN apt-get install -y --no-install-recommends curl xz-utils gnupg netbase zlib1g-dev
RUN apt-get install -y --no-install-recommends pcre2-utils
RUN rm -rf /var/lib/apt/lists/*

# Swift
WORKDIR /build
RUN curl -O https://download.swift.org/swift-5.6.1-release/ubuntu2004/swift-5.6.1-RELEASE/swift-5.6.1-RELEASE-ubuntu20.04.tar.gz
RUN tar -xzf  swift-5.6.1-RELEASE-ubuntu20.04.tar.gz
WORKDIR /build/swift-5.6.1-RELEASE-ubuntu20.04/usr
RUN mkdir /opt/swift
RUN cp -r * /opt/swift

# Java 
WORKDIR /build
RUN apt-get update
RUN apt-get install -y --no-install-recommends software-properties-common
RUN add-apt-repository ppa:linuxuprising/java
RUN echo debconf shared/accepted-oracle-license-v1-3 select true | debconf-set-selections
RUN echo debconf shared/accepted-oracle-license-v1-3 seen true | debconf-set-selections
RUN apt-get install -y --no-install-recommends oracle-java17-installer
RUN apt-get install -y --no-install-recommends oracle-java17-set-default
RUN apt-get install -y --no-install-recommends libedit2 libz3-dev
RUN apt-get install -y --no-install-recommends time

# Haskell
WORKDIR /build
RUN apt-get install -y --no-install-recommends ghc cabal-install
RUN cabal update
RUN cabal install parallel

# OCaml (multicore)
WORKDIR /build
RUN apt-get install -y --no-install-recommends opam
RUN opam init -y --disable-sandboxing 
RUN opam update -y
RUN opam switch -y create 4.14.0 --repositories=multicore=git+https://github.com/ocaml-multicore/multicore-opam.git,default 

# Stack
WORKDIR /build
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN git clone --recursive https://github.com/koka-lang/koka -b dev-icfp22

# Koka v2.3.3 
WORKDIR /build
RUN git clone --recursive https://github.com/koka-lang/koka -b v2.3.3 koka-v2.3.3
WORKDIR /build/koka-v2.3.3
RUN stack build
RUN stack exec koka -- -e util/bundle -- -i --prefix=./local

# Koka v2.3.3 no-reuse
WORKDIR /build
RUN git clone --recursive https://github.com/koka-lang/koka -b v2.3.3-old koka-v2.3.3-old
WORKDIR /build/koka-v2.3.3-old
RUN stack build
RUN stack exec koka -- -e util/bundle -- -i --prefix=./local

# Koka
WORKDIR /build/koka
RUN stack build
# RUN stack exec koka -- -e util/bundle -- --postfix=docker
# RUN util/install.sh -f -b bundle/v2.4.1/koka-docker.tar.gz

# Benchmarks
WORKDIR /build/koka/test/bench
RUN mkdir build
WORKDIR /build/koka/test/bench/build
RUN cmake .. -DCMAKE_BUILD_TYPE=Release

SHELL ["/bin/bash", "-c"]
RUN eval $(opam env) && cmake --build .
RUN echo "ulimit -s unlimited" >> ~/.bashrc 
RUN opam env >> ~/.bashrc
