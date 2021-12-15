FROM alpine:3.13

# The koka source should be mounted here readonly
# It will use overlays to build the bundle and then export it the output directory
# So /output should be mounted as readwrite
VOLUME /code
VOLUME /output 

# Install all the necessary packages
RUN apk update
RUN apk add bash curl zip unzip tar util-linux
RUN apk add alpine-sdk build-base cmake ninja
RUN apk add gmp-dev

# Alpine compat fixes
RUN apk add --no-cache --upgrade grep
RUN ln -s /usr/lib/libncursesw.so.6 /usr/lib/libtinfo.so.6

RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack update

RUN git clone https://github.com/Microsoft/vcpkg.git ~/vcpkg
RUN ~/vcpkg/bootstrap-vcpkg.sh

# Add and run the builder script specifying the postfix of the bundle
ADD ./builder.sh /builder.sh
RUN chmod +x /builder.sh

ENTRYPOINT [ "/builder.sh" ]

CMD [ "alpine" ]