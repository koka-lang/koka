FROM rockylinux/rockylinux:8.5

# The koka source should be mounted here readonly
# It will use overlays to build the bundle and then export it the output directory
# So /output should be mounted as readwrite
VOLUME /code
VOLUME /output 

# Install all the necessary packages
RUN dnf update -y
RUN dnf groupinstall "Development Tools" -y
RUN dnf install gmp-devel -y

RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack update

RUN git clone https://github.com/Microsoft/vcpkg.git ~/vcpkg
RUN ~/vcpkg/bootstrap-vcpkg.sh

# Add and run the builder script specifying the postfix of the bundle
ADD ./builder.sh /builder.sh
RUN chmod +x /builder.sh

ENTRYPOINT [ "/builder.sh" ]

CMD [ "rhel" ]