# This container is a wrapper for the FPM program

FROM ruby:3
RUN apt update
RUN apt install -y build-essential libffi-dev rpm libarchive-tools zstd squashfs-tools

RUN gem install fpm

ENV SHELL=/bin/bash

VOLUME /source
VOLUME /build

ENTRYPOINT ["fpm"]