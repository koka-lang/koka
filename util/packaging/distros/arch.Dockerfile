FROM manjarolinux/base:20220109

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

# Arch has latest version anyway, so no need for install script
RUN pacman -Sy --noconfirm ghc ghc-static cabal-install
RUN cabal update

# Add and run the builder script specifying the postfix of the bundle
ADD ./builder.sh /builder.sh
RUN chmod +x /builder.sh

ENTRYPOINT [ "/builder.sh" ]

CMD [ "arch", "cabal" ]