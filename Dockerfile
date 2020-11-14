# Created by @Lassik
# Build and run as:
# > docker build -t <mytag> .
# > docker run -it <mytag>

FROM haskell:8.6.5 AS build
RUN mkdir -p ~/.local/bin \
 && cp /usr/local/bin/stack ~/.local/bin/stack \
 && find /usr/local -type f -delete
RUN apt-get update \
 && apt-get install -y --no-install-recommends cmake \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /build
RUN git clone --recursive https://github.com/koka-lang/koka -b dev
WORKDIR /build/koka
RUN stack build
RUN stack exec koka -- util/install -- --prefix=/usr/local

FROM debian:stretch
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      gcc libc-dev \
      cmake make ninja-build \
      nodejs ca-certificates \
 && rm -rf /var/lib/apt/lists/*
COPY --from=build /usr/local/ /usr/local/
CMD ["koka"]
