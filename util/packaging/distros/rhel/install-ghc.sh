#!/bin/bash

# This is the last release we can bootstrap
CURRENT_CABAL_SOURCE="https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0.tar.gz"

install_deps() {
  dnf install -y epel-release
  dnf install -y which wget bind-utils
  dnf install -y ghc-devel ghc-Cabal-devel zlib-devel ghc-template-haskell-devel
}

download_source() {
  wget -O cabal-install.tar.gz "$CURRENT_CABAL_SOURCE"
  tar -xf cabal-install.tar.gz
  rm cabal-install.tar.gz
}

main() {
  set -e # Exit on error

  install_deps

  mkdir build
  cd build

  download_source
  cd cabal-install-*

  ./bootstrap.sh --global -j "$(nproc)"

  cd ../..
  rm -rf build
}

main
