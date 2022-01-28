#!/bin/sh

get_latest_release() {
  api_res=$(curl --silent "https://api.github.com/repos/$1/releases/latest")
  download_urls=$(echo $api_res | jq -r ".assets[].browser_download_url")
  echo "$download_urls"
}

install_ghc_aarch64() {
  apk add jq

  mkdir apks
  cd apks

  urls=$(get_latest_release "rubikscraft/alpine-ghc-aarch64")
  for url in $urls; do
    echo "Downloading $url"
    wget $url
  done

  apk add --allow-untrusted ./*.apk

  cd ..
  rm -r apks
}

install_ghc_main() {
  apk add ghc cabal
}

main() {
  set -e # Exit on error

  if [ "$(uname -m)" = "aarch64" ]; then
    install_ghc_aarch64
  else
    install_ghc_main
  fi
}

main
