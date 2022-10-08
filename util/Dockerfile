# Create a docker image with a working Koka compiler (including sources).
#
# Build:
# > docker build -t <mytag> .
#
# To Run:
# > docker run -it <mytag>
# or
# > docker run -v <local-dir> -it <mytag>
#
# To start with a shell prompt:
# > docker run -it <mytag> bash
#
# To publish, use a tag like `kokalang/koka:v2.x.x`
# > docker push <mytag>

FROM haskell:8.10.7 AS build
ENV KOKAVER=dev

RUN mkdir -p ~/.local/bin
RUN cp /usr/local/bin/stack ~/.local/bin/stack
RUN find /usr/local -type f -delete
RUN apt-get update
RUN apt-get install -y --no-install-recommends ca-certificates
RUN apt-get install -y --no-install-recommends libc-dev build-essential tar cmake
RUN apt-get install -y --no-install-recommends gcc curl

# Build Koka
WORKDIR /build
RUN git clone --recursive https://github.com/koka-lang/koka -b ${KOKAVER}
WORKDIR /build/koka
RUN stack build

# For installing C libraries (pcre2) we use Conan
RUN apt-get install -y --no-install-recommends python3-pip
RUN pip3 install setuptools wheel 
RUN pip3 install conan

# Build Koka install bundle
RUN stack exec koka -- -e util/bundle -- --postfix=docker --prefix=bundle/docker

# Install 
WORKDIR /usr/local 
RUN tar -xzvf /build/koka/bundle/koka-docker.tar.gz     

# Entry
WORKDIR /build/koka
CMD ["koka"]
