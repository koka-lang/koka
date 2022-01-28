#!/bin/bash

CABAL_VERSION="3.6.2.0"

ARCH=$(uname -m)
CURRENT_CABAL_BIN="https://downloads.haskell.org/cabal/cabal-install-$CABAL_VERSION/cabal-install-$CABAL_VERSION-$ARCH-linux-deb10.tar.xz"

install_deps() {
  dnf install -y epel-release
  dnf install -y which wget bind-utils numactl
  dnf install -y ghc-devel ghc-Cabal-devel zlib-devel ghc-template-haskell-devel
}

install_cabal() {
  wget -O cabal-bin.tar.gz "$CURRENT_CABAL_BIN"
  tar -xf cabal-bin.tar.gz -C /usr/bin/
  rm cabal-bin.tar.gz
}

main() {
  set -e # Exit on error

  install_deps

  mkdir cabal
  cd cabal

  install_cabal

  cd ..
  rm -rf cabal
}

main
