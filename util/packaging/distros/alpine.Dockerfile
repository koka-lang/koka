# Only latest version supported, too much hassle to get it compatible with older versions
# 1. It really wants to dynamically link libffi into it, this package has a different version on alpine 3.14 and before
FROM alpine:3.15.0

# The koka source should be mounted here readonly
# It will use overlays to build the bundle and then export it the output directory
# So /output should be mounted as readwrite
VOLUME /code
VOLUME /output 

# Install all the necessary packages
RUN apk update
# Alpine compat fixes
RUN apk add --no-cache --upgrade grep wget util-linux
RUN ln -s /usr/lib/libncursesw.so.6 /usr/lib/libtinfo.so.6

# Build tools
RUN apk add alpine-sdk linux-headers libffi-dev cmake

# Conan
RUN apk add py3-pip
RUN pip3 install conan

# Easy hackage update trigger
ARG UPDATE_HACKAGE=1

# Ghc is actually being added to aarch64 in alpine, lets goooo
# https://gitlab.alpinelinux.org/alpine/aports/-/issues/11176
ADD ./alpine/*.sh /helpers/
RUN /helpers/install-ghc.sh
RUN cabal update

# Add and run the builder script specifying the postfix of the bundle
ADD ./builder.sh /builder.sh
RUN chmod +x /builder.sh

ENTRYPOINT [ "/builder.sh" ]

CMD [ "alpine", "cabal" ]