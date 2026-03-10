# Dockerfile for the artifact:
#   "Syntactic Implicit Parameters with Static Overloading"
#
# Build this image:
#   docker build -t pldi26-implicits .
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
RUN apt-get install -y --no-install-recommends curl xz-utils build-essential
RUN apt-get install -y --no-install-recommends libffi-dev libgmp-dev zlib1g-dev

# Install Stack (Haskell build tool)
RUN curl -sSL https://get.haskellstack.org/ | /bin/bash
RUN stack update

# Clone and build the Koka compiler (artifact branch)
WORKDIR /artifact
RUN git clone --recursive https://github.com/koka-lang/koka -b artifact/syntactic-implicits
WORKDIR /artifact/koka
RUN stack build :koka-plain

# Compile and run all artifact examples, verifying output against pre-saved
# expected results.  The script exits non-zero if any example fails, which
# causes the Docker build to fail immediately.
WORKDIR /artifact/koka
RUN bash test/artifact/pldi26/check.sh

CMD ["/bin/bash"]
