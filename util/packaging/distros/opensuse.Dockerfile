FROM opensuse/leap:15.3

# The koka source should be mounted here readonly
# It will use overlays to build the bundle and then export it the output directory
# So /output should be mounted as readwrite
VOLUME /code
VOLUME /output 

# Install all the necessary packages
RUN zypper update -y
RUN zypper install -y curl wget zip unzip tar git
RUN zypper install -y -t pattern devel_C_C++
RUN zypper install -y gcc-c++ cmake
# Why is gcc-c++ not in the devel_C_C++ pattern?

# Conan
RUN zypper install -y python3-pip
RUN pip3 install conan

# Easy hackage update trigger
ARG UPDATE_HACKAGE=1

RUN zypper install -y ghc cabal-install
RUN cabal update

# Add and run the builder script specifying the postfix of the bundle
ADD ./builder.sh /builder.sh
RUN chmod +x /builder.sh

ENTRYPOINT [ "/builder.sh" ]

CMD [ "opensuse", "cabal" ]