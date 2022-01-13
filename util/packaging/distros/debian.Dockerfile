FROM debian:10.11

# The koka source should be mounted here readonly
# It will use overlays to build the bundle and then export it the output directory
# So /output should be mounted as readwrite
VOLUME /code
VOLUME /output 

# Install all the necessary packages
RUN apt update -y
RUN apt install -y build-essential pkg-config cmake
RUN apt install -y curl wget zip unzip tar

# Conan
RUN apt install -y python3-pip
RUN pip3 install conan

# Easy hackage update trigger
ARG UPDATE_HACKAGE=1

RUN apt install -y ghc cabal-install
RUN cabal update

# Add and run the builder script specifying the postfix of the bundle
ADD ./builder.sh /builder.sh
RUN chmod +x /builder.sh

ENTRYPOINT [ "/builder.sh" ]

CMD [ "debian", "cabal" ]