FROM opensuse/leap:15.3

# The koka source should be mounted here readonly
# It will use overlays to build the bundle and then export it the output directory
# So /output should be mounted as readwrite
VOLUME /code
VOLUME /output 

# Install all the necessary packages
RUN zypper update -y
RUN zypper install -y curl wget zip unzip tar git
RUN zypper install -t pattern -y devel_C_C++
RUN zypper install -y gmp-devel gcc-c++
# Why is gcc-c++ not in the devel_C_C++ pattern?

RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack update

RUN git clone https://github.com/Microsoft/vcpkg.git ~/vcpkg
RUN ~/vcpkg/bootstrap-vcpkg.sh

# Add and run the builder script specifying the postfix of the bundle
ADD ./builder.sh /builder.sh
RUN chmod +x /builder.sh

ENTRYPOINT [ "/builder.sh" ]

CMD [ "opensuse" ]