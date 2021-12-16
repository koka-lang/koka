#!/bin/sh

# This script is ran from inside the distro specific docker containers
# It automates the building of koka with stack and then cleans everything except the bundles

DISTRO=""
BUILD_MODE=""

mount_overlay() {
  echo "Mounting overlay"
  # Check if /proc/filesystems contains overlayfs and tmpfs
  if ! grep -q overlay /proc/filesystems || ! grep -q tmpfs /proc/filesystems; then
    echo "Your system does not support overlayfs or tmpfs, it needs this to build"
    exit 1
  fi

  mkdir -p /tmp/overlay
  
  mount -t tmpfs tmpfs /tmp/overlay

  if [ $? -ne 0 ]; then
    echo "Failed to mount tmpfs"
    exit 1
  fi

  mkdir -p /tmp/overlay/coderw /tmp/overlay/codework

  mount -t overlay overlay -o lowerdir=/code,upperdir=/tmp/overlay/coderw,workdir=/tmp/overlay/codework /tmp/overlay/coderw
  if [ $? -ne 0 ]; then
    echo "Failed to mount overlayfs, the container needs the SYS_ADMIN capability"
    exit 1
  fi

  cd /tmp/overlay/coderw

  echo "Overlay mounted"
}

build_koka_stack() {
  echo "Building koka with stack"
  stack build

  echo "Making bundle"
  #stack exec koka -- -e util/bundle -- --postfix="$DISTRO"
  # Bypass bug in koka
  script --return --quiet -c "stack exec koka -- -e util/bundle -- --postfix=\"$DISTRO\"" /dev/null
  if [ $? -ne 0 ]; then
    echo "Failed to build koka"
    exit 1
  fi

  echo "Built koka with stack"
}

build_koka_cabal() {
  echo "Building koka with cabal"
  cabal new-build --enable-executable-static

  echo "Making bundle"
  script --return --quiet -c "cabal new-run koka -- -e util/bundle -- --postfix=\"$DISTRO\"" /dev/null
  if [ $? -ne 0 ]; then
    echo "Failed to build koka"
    exit 1
  fi

  echo "Built koka with cabal"
}

export_build() {
  echo "Exporting build"

  cp ./bundle/*.tar.gz /output/

  if [ $? -ne 0 ]; then
    echo "Failed to export bundle"
    exit 1
  fi

  echo "Exported build"
}

full_build() {
  echo "Starting build"

  mount_overlay

  if [ "$BUILD_MODE" == "stack" ]; then
    build_koka_stack
  elif [ "$BUILD_MODE" == "cabal" ]; then
    build_koka_cabal
  fi

  export_build

  echo "Build finished"
}

init_parse_param() {
  # First param is the distro, if exists
  if [ -n "$1" ]; then
    DISTRO="$1"
  fi

  # Second param is BUILD_MODE, if exists
  if [ -n "$2" ]; then
    BUILD_MODE="$2"
  fi

  # Default distro is unknown
  if [ -z "$DISTRO" ]; then
    DISTRO="unknown"
  fi

  # Default build mode is stack
  if [ -z "$BUILD_MODE" ]; then
    BUILD_MODE="stack"
  fi

  echo "Distro: $DISTRO"
  echo "Build mode: $BUILD_MODE"

  full_build
}

init_parse_param "$@"
