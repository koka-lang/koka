FROM manjarolinux/base:20220116

# The koka source should be mounted here readonly
# It will use overlays to build the bundle and then export it the output directory
# So /output should be mounted as readwrite
VOLUME /code
VOLUME /output 

# Install all the necessary packages
RUN pacman -Syu --noconfirm
RUN pacman -Sy --noconfirm base-devel cmake
RUN pacman -Sy --noconfirm curl zip unzip tar 

# Conan
RUN pacman -Sy --noconfirm python-pip
RUN pip install conan

# Easy hackage update trigger
ARG UPDATE_HACKAGE=1

# Nobody needs ghc compiled for aarch64 right? Nobody uses aarch64 right?
ADD ./arch/*.sh /helpers/
RUN /helpers/install-ghc.sh
RUN cabal update

# Add and run the builder script specifying the postfix of the bundle
ADD ./builder.sh /builder.sh
RUN chmod +x /builder.sh

ENTRYPOINT [ "/builder.sh" ]

CMD [ "arch", "cabal" ]