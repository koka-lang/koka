FROM archlinux:base-20211212.0.41353

# The koka source should be mounted here readonly
# It will use overlays to build the bundle and then export it the output directory
# So /output should be mounted as readwrite
VOLUME /code
VOLUME /output 

# Install all the necessary packages
RUN pacman -Sy --noconfirm
RUN pacman -Sy base-devel --noconfirm
RUN pacman -Sy curl zip unzip tar --noconfirm
# Arch has latest version anyway
RUN pacman -Sy stack --noconfirm

RUN stack update

RUN git clone https://github.com/Microsoft/vcpkg.git ~/vcpkg
RUN ~/vcpkg/bootstrap-vcpkg.sh

# Add and run the builder script specifying the postfix of the bundle
ADD ./builder.sh /builder.sh
RUN chmod +x /builder.sh

ENTRYPOINT [ "/builder.sh" ]

CMD [ "arch" ]