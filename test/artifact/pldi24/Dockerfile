# Create a docker image for running the benchmarks.
# Build:
# > docker build -t <mytag> .
#
# To Run:
# > docker run -it <mytag>
# or
# > docker run -v <local-dir> -it <mytag>
#
# `mytag` has the form `repo/name:tag`, like `daanx/pldi24-tree`

FROM ubuntu:22.04
RUN apt-get update
RUN apt-get install -y --no-install-recommends ca-certificates
RUN apt-get install -y --no-install-recommends git cmake make
RUN apt-get install -y --no-install-recommends clang gcc libc-dev
RUN apt-get install -y --no-install-recommends curl xz-utils build-essential bc tar
RUN apt-get install -y --no-install-recommends pcre2-utils
RUN apt-get update

# Mimalloc 2.1.2
WORKDIR /artifact
RUN git clone https://github.com/microsoft/mimalloc -b v2.1.2
RUN mkdir -p mimalloc/out/release
WORKDIR /artifact/mimalloc/out/release
RUN cmake ../..
RUN make -j
RUN make install

# OCaml (multicore)
WORKDIR /artifact
RUN apt-get install -y --no-install-recommends ocaml

# GHC
WORKDIR /artifact
RUN apt-get install -y --no-install-recommends ghc

# Stack
WORKDIR /artifact
RUN curl -sSL https://get.haskellstack.org/ | /bin/bash
RUN stack update

# Koka
WORKDIR /artifact
RUN git clone --recursive https://github.com/koka-lang/koka  -b artifact-pldi24
WORKDIR /artifact/koka
RUN stack build

# Build benchmarks
RUN apt-get install -y --no-install-recommends bsdextrautils time
SHELL ["/bin/bash", "-c"]
WORKDIR /artifact/koka/test/artifact/pldi24
RUN ./bench.sh build allb



