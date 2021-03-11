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
RUN apt-get install -y --no-install-recommends gcc libc-dev
RUN apt-get install -y --no-install-recommends ghc ocaml
RUN apt-get install -y --no-install-recommends ghc ocaml
RUN apt-get install -y --no-install-recommends curl xz-utils gnupg netbase zlib1g-dev
RUN apt-get install -y --no-install-recommends build-essential tar
RUN rm -rf /var/lib/apt/lists/*
WORKDIR /build
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN git clone --recursive https://github.com/koka-lang/koka -b v2.1.1
WORKDIR /build/koka
RUN stack build
RUN stack exec koka -- util/bundle -- --postfix=docker
RUN util/install.sh -f -b dist/koka-docker.tar.gz
WORKDIR /build
RUN curl -O https://swift.org/builds/swift-5.3.3-release/ubuntu2004/swift-5.3.3-RELEASE/swift-5.3.3-RELEASE-ubuntu20.04.tar.gz
RUN tar -xzf  swift-5.3.3-RELEASE-ubuntu20.04.tar.gz
WORKDIR /build/swift-5.3.3-RELEASE-ubuntu20.04/usr
RUN mkdir /opt/swift
RUN cp -r * /opt/swift
WORKDIR /build
RUN apt-get update
RUN apt-get install -y --no-install-recommends software-properties-common
RUN add-apt-repository ppa:linuxuprising/java
RUN echo debconf shared/accepted-oracle-license-v1-2 select true | debconf-set-selections
RUN echo debconf shared/accepted-oracle-license-v1-2 seen true | debconf-set-selections
RUN apt-get install -y --no-install-recommends oracle-java15-installer
RUN apt-get install -y --no-install-recommends oracle-java15-set-default
RUN apt-get install -y --no-install-recommends libedit2 libz3-dev
WORKDIR /build/koka/test/bench
RUN mkdir build
WORKDIR /build/koka/test/bench/build
RUN cmake .. -DCMAKE_BUILD_TYPE=Release
RUN cmake --build .