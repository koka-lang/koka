FROM rockylinux/rockylinux:8

VOLUME [ "/data" ]

RUN dnf update -y
RUN dnf groupinstall "Development Tools" -y
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack update

RUN dnf install gmp-devel
RUN git clone https://github.com/Microsoft/vcpkg.git ~/vcpkg
RUN ~/vcpkg/bootstrap-vcpkg.sh

ADD ./builder.sh /builder.sh
RUN chmod +x /builder.sh

WORKDIR /data

ENTRYPOINT [ "/builder.sh" ]

CMD [ "rhel" ]