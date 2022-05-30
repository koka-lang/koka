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
# RUN rm -rf /var/lib/apt/lists/*
# RUN apt-get update

# Swift
WORKDIR /build
RUN curl -O https://download.swift.org/swift-5.6.1-release/ubuntu2004/swift-5.6.1-RELEASE/swift-5.6.1-RELEASE-ubuntu20.04.tar.gz
RUN tar -xzf  swift-5.6.1-RELEASE-ubuntu20.04.tar.gz
WORKDIR /build/swift-5.6.1-RELEASE-ubuntu20.04/usr
RUN mkdir /opt/swift
RUN cp -r * /opt/swift

# Java 
WORKDIR /build
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

# Vcpkg 
WORKDIR /root
RUN apt-get install -y --no-install-recommends zip unzip pkg-config
RUN apt-get install -y --no-install-recommends clang
RUN git clone https://github.com/Microsoft/vcpkg.git
RUN ./vcpkg/bootstrap-vcpkg.sh
RUN ./vcpkg/vcpkg install pcre2

# Koka v2.3.3 no-reuse
WORKDIR /build
RUN git clone --recursive https://github.com/koka-lang/koka -b v2.3.3-old koka-v2.3.3-old
WORKDIR /build/koka-v2.3.3-old
RUN stack build
RUN mkdir .koka
# RUN stack exec koka -- --version 
# RUN stack exec koka -- -e util/bundle -- -i --prefix=./local

# Koka v2.3.3 
WORKDIR /build
RUN git clone --recursive https://github.com/koka-lang/koka -b v2.3.3 koka-v2.3.3
WORKDIR /build/koka-v2.3.3
RUN stack build
RUN mkdir .koka

# Koka
WORKDIR /build
RUN git clone --recursive https://github.com/koka-lang/koka -b dev-icfp22
WORKDIR /build/koka
RUN stack build 
RUN mkdir .koka
RUN stack exec koka -- --version
RUN stack exec koka -- -e util/bundle -- --postfix=docker
RUN util/install.sh -f -b bundle/v2.4.1/koka-docker.tar.gz

# Benchmarks
WORKDIR /build/koka/test/bench
RUN mkdir build
WORKDIR /build/koka/test/bench/build
RUN cmake .. -DCMAKE_BUILD_TYPE=Release

SHELL ["/bin/bash", "-c"]
RUN echo "ulimit -s unlimited" >> ~/.bashrc 
RUN opam env >> ~/.bashrc

# --------------------------------------------------------------
# Now do the following manually:
# (somehow does not work in the script :-( )
# --------------------------------------------------------------

# WORKDIR /build/koka-v2.3.3
# RUN stack exec koka -- --version 
# RUN stack exec koka -- -e util/bundle -- -i --prefix=./local

# WORKDIR /build/koka-v2.3.3-old
# RUN stack build --fast
# RUN stack exec koka -- -e util/bundle -- -i --prefix=./local

# WORKDIR /build/koka/test/bench/build
# RUN eval $(opam env)
# RUN cmake --build .

# RUN /build/koka-v2.3.3/local/bin/koka --target=c -O2 -v -r --buildname=kkfbip-rbtree --outputdir=koka/outfbip/bench/ ../koka/rbtree-fbip.kk 
# RUN /build/koka-v2.3.3/local/bin/koka --target=c -O2 -v -r --buildname=kkfbip-rbtree-ck --outputdir=koka/outfbip/bench/ ../koka/rbtree-fbip-ck.kk
# RUN /build/koka-v2.3.3/local/bin/koka --target=c -O2 -v -r --buildname=kkfbip-binarytrees --outputdir=koka/outfbip/bench/ ../koka/binarytrees-fbip.kk

# # and run all tests:
# RUN koka -e ../bench.kk -- --iter=10 --norm 

# # or just some:
# RUN koka -e ../bench.kk -- --iter=3 --norm --test=rbtree,binarytrees --lang=kk,kkfbip,cpp,ml
 