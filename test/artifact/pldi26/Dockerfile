# Dockerfile for the artifact:
#   "Syntactic Implicit Parameters with Static Overloading"
#
# Build this image, go to the root of the koka installation (`~/dev/koka` for example)
# as it copies that koka directory into the docker image.
#   docker build -t pldi26-implicits -f test/artifact/pldi26/Dockerfile .
#
# Run interactively:
#   docker run -it pldi26-implicits
#
# The working directory inside the container will be the koka root.
# Run examples with:
#   stack exec koka-plain -- -e test/artifact/pldi26/examples/intro.kk
FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update
RUN apt-get install -y --no-install-recommends ca-certificates
RUN apt-get install -y --no-install-recommends git cmake make
RUN apt-get install -y --no-install-recommends clang gcc libc-dev
RUN apt-get install -y --no-install-recommends build-essential pkg-config curl
RUN apt-get install -y --no-install-recommends tar zip unzip xz-utils
RUN apt-get install -y --no-install-recommends libffi-dev libgmp-dev zlib1g-dev pcre2-utils
RUN apt-get install -y --no-install-recommends vim
RUN apt-get update

# Install Stack (Haskell build tool)
WORKDIR /root
RUN curl -sSL https://get.haskellstack.org/ | /bin/bash
RUN stack update

# Install vcpkg
RUN git clone https://github.com/microsoft/vcpkg.git
WORKDIR /root/vcpkg
RUN ./bootstrap-vcpkg.sh

# Copy and build the Koka compiler (artifact branch)
WORKDIR /root
COPY . /root/koka
# Or checkout the Koka compiler from github
# WORKDIR /root
# RUN git clone --recursive https://github.com/koka-lang/koka  -b artifact/syntactic-implicits koka

WORKDIR /root/koka
RUN stack build --fast

# Compile the standard libraries
RUN stack exec koka -- util/link-std.kk

# Environment
SHELL ["/bin/bash", "-c"]
CMD ["/bin/bash"]
