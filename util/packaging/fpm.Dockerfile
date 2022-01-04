# This container is a wrapper for the FPM program

FROM ruby:3
RUN apt update
RUN apt install -y build-essential libffi-dev rpm libarchive-tools zstd squashfs-tools

RUN gem install fpm

# Patch fpm
RUN sed -i 's/full_record_path = add_paxstring(full_record_path)/# full_record_path = add_paxstring(full_record_path)/' /usr/local/bundle/gems/fpm-*/lib/fpm/package/apk.rb

ENV SHELL=/bin/bash

VOLUME /source
VOLUME /build
VOLUME /scripts

ENTRYPOINT ["/bin/bash"]