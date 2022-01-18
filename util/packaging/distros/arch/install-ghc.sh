#!/bin/bash

GHC_VERSION="9.0.2"
CABAL_VERSION="3.6.0.0"

ARCH=$(uname -m)
CURRENT_GHC_BIN="https://downloads.haskell.org/ghc/$GHC_VERSION/ghc-$GHC_VERSION-$ARCH-deb10-linux.tar.xz"
CURRENT_CABAL_BIN="https://downloads.haskell.org/cabal/cabal-install-$CABAL_VERSION/cabal-install-$CABAL_VERSION-$ARCH-linux-deb10.tar.xz"

install_deps() {
  pacman -Sy --noconfirm numactl wget
}

install_ghc() {
  wget -O ghc-bin.tar.gz "$CURRENT_GHC_BIN"
  tar -xf ghc-bin.tar.gz
  cd ghc-$GHC_VERSION

  ./configure --prefix=/usr
  make install -j$(nproc)

  cd ..
  rm -r ghc-$GHC_VERSION
  rm ghc-bin.tar.gz
}

install_cabal() {
  wget -O cabal-bin.tar.gz "$CURRENT_CABAL_BIN"
  tar -xf cabal-bin.tar.gz -C /usr/bin/
  rm cabal-bin.tar.gz
}

install_aarch64() {
  install_deps

  mkdir cabal
  cd cabal

  install_ghc

  install_cabal

  cd ..
  rm -rf cabal
}

install_main() {
  pacman -Sy --noconfirm ghc ghc-static cabal-install
}

main() {
  set -e # Exit on error

  if [ "$ARCH" = "aarch64" ]; then
    install_aarch64
  else
    install_main
  fi
}

main