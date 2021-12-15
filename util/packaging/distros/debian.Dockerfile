FROM debian:10

# The koka source should be mounted here readonly
# It will use overlays to build the bundle and then export it the output directory
# So /output should be mounted as readwrite
VOLUME /code
VOLUME /output 

# Install all the necessary packages
RUN apt update -y
RUN apt install build-essential -y
RUN apt install libgmp-dev curl wget zip unzip tar pkg-config -y

RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack update

RUN git clone https://github.com/Microsoft/vcpkg.git ~/vcpkg
RUN ~/vcpkg/bootstrap-vcpkg.sh

# Add and run the builder script specifying the postfix of the bundle
ADD ./builder.sh /builder.sh
RUN chmod +x /builder.sh

ENTRYPOINT [ "/builder.sh" ]

CMD [ "debian" ]