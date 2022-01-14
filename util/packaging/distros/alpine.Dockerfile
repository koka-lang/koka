# Only latest version supported, too much hassle to get it compatible with older versions
# 1. Stack first of all just doesnt work on alpine, so we need to use cabal
# 2. It really wants to dynamically link libffi into it, this package has a different version on alpine 3.14 and before
# 3. Vcpkg cant dynamically download cmake packages because of musl, so we need to use the one from the alpine image
# 4. The alpine cmake package is too old on any version below 3.15
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

# Cabal
ADD ./alpine/install-ghc.sh ./
RUN ./install-ghc.sh
RUN cabal update

# Add and run the builder script specifying the postfix of the bundle
ADD ./builder.sh /builder.sh
RUN chmod +x /builder.sh

ENTRYPOINT [ "/builder.sh" ]

CMD [ "alpine", "cabal" ]