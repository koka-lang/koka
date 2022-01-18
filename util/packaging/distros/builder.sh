#!/bin/sh

# This script is ran from inside the distro specific docker containers
# It automates the building of koka with stack and then cleans everything except the bundles

DISTRO=""
BUILD_MODE=""

LIBC_VERSION=""
KOKA_VERSION=""
ARCHITECTURE=""

# If you change this, change it in ../package.sh too
PACKAGE_PREFIX="/usr/local"

CLEAN_FOLDERS=".koka .stack-work dist dist-newstyle"
BUNDLE_LIBRARIES="libffi libgmp libnuma"
CABAL_FLAGS="-O2 --disable-debug-info --enable-executable-stripping --enable-library-stripping"
STACK_FLAGS=""

LOG_PREFIX="[KOKA INTERNAL BUILDER] "
METADATA_DIR="./extra-meta"
LIBRARY_DIR="./extra-libs"

info() {
  echo "$LOG_PREFIX$@"
}

warn() {
  echo "$LOG_PREFIX$@" >&2
}

stop() {
  warn $@
  exit 1
}

#---------------------------------------------------------
# LIBC
#---------------------------------------------------------

is_libc_musl() {
  libcversion=$(ldd --version 2>&1 | head -n 1)
  if echo $libcversion | grep -q musl; then
    return 0
  else
    return 1
  fi
}

get_libc_version() {
  libcversion=""
  if ! is_libc_musl; then
    libcversion=$(ldd --version 2>&1 | head -n 1)
  else
    libcversion=$(ldd --version 2>&1 | tail -n +2 | head -n 1)
  fi

  libcversion=$(echo $libcversion | awk '{print $NF}')
  echo $libcversion
}

#---------------------------------------------------------
# KOKA INFO
#---------------------------------------------------------

get_koka_version_from_cabal() {
  KOKA_VERSION="v$(cat ./koka.cabal | grep "^version:" | awk '{print $2}')"
}

get_koka_arch() {
  # We need this piece of code to get the architecture of the koka binary
  export koka_code="\n:l std/os/env\nprintln(\"KK_arch: \" ++ get-cpu-arch())"

  if [ "$BUILD_MODE" = "stack" ]; then
    kk_arch=$(echo -e "$koka_code" | stack exec koka)
  elif [ "$BUILD_MODE" = "cabal" ]; then
    kk_arch=$(echo -e "$koka_code" | cabal new-run koka)
  fi
  unset koka_code

  kk_arch=$(echo "$kk_arch" | grep -Pom 1 "(?<=KK_arch: )[a-zA-Z0-9]+")
  echo "$kk_arch"
}

get_koka_bin_location() {
  koka_bin_loc=""

  if [ "$BUILD_MODE" = "stack" ]; then
    koka_bin_loc=$(stack exec koka -- --version | grep "^bin")
  elif [ "$BUILD_MODE" = "cabal" ]; then
    koka_bin_loc=$(cabal new-run koka -- --version | grep "^bin")
  fi

  koka_bin_loc=$(echo $koka_bin_loc | awk '{print $NF}')

  echo "$koka_bin_loc/koka"
}

get_koka_bin_deps() {
  koka_bin_loc=$1

  if [ -z "$koka_bin_loc" ]; then
    stop "Failed to get koka bin location"
  fi

  koka_bin_deps=$(ldd $koka_bin_loc | grep "=>" | awk '{print $3}')

  echo $koka_bin_deps
}

#---------------------------------------------------------
# BUILD ENV PREPARE
#---------------------------------------------------------

clean_workdir() {
  info "Cleaning up"

  for folder in $CLEAN_FOLDERS; do
    if [ -d "$folder" ]; then
      find "$folder" ! -type d -exec rm '{}' \;
    fi
  done
}

mount_overlay() {
  info "Mounting overlay"
  # Check if /proc/filesystems contains overlayfs and tmpfs
  if ! grep -q overlay /proc/filesystems || ! grep -q tmpfs /proc/filesystems; then
    stop "Your system does not support overlayfs or tmpfs, it needs this to build"
  fi

  mkdir -p /tmp/overlay

  mount -t tmpfs tmpfs /tmp/overlay

  if [ $? -ne 0 ]; then
    stop "Failed to mount tmpfs"
  fi

  mkdir -p /tmp/overlay/source-work /tmp/overlay/source-upper /tmp/overlay/source-merged

  mount -t overlay overlay \
    -o lowerdir=/code,upperdir=/tmp/overlay/source-upper,workdir=/tmp/overlay/source-work \
    /tmp/overlay/source-merged
  if [ $? -ne 0 ]; then
    stop "Failed to mount overlayfs, the container needs the SYS_ADMIN capability"
  fi

  cd /tmp/overlay/source-merged

  info "Overlay mounted"
}

#---------------------------------------------------------
# BUILDING
#---------------------------------------------------------

build_koka() {
  info "Building koka"

  status=1
  if [ "$BUILD_MODE" = "stack" ]; then
    stack build $STACK_FLAGS
    status=$?
  elif [ "$BUILD_MODE" = "cabal" ]; then
    extra_flags=""

    # LINK STATICALLY ON MUSL
    if is_libc_musl; then
      extra_flags="$extra_flags --enable-executable-static"
      echo yes >"$METADATA_DIR/static"
    fi

    # MAKE SURE DYNAMIC LIBRARIES ARE FOUND
    if [ -n "$KOKA_VERSION" ]; then
      dynamic_libs_loc="$PACKAGE_PREFIX/lib/koka/$KOKA_VERSION/libs"
      extra_flags="$extra_flags --ghc-option=-optl-Wl,-rpath=$dynamic_libs_loc"
    fi

    cabal new-configure $CABAL_FLAGS $extra_flags
    cabal new-build
    status=$?
  fi

  if [ $status -ne 0 ]; then
    stop "Failed to build koka"
  fi

  info "Koka built"
}

bundle_koka() {
  info "Bundling koka"

  # COLLECT LIBRARY DEPENDENCIES
  koka_bin_loc=$(get_koka_bin_location)
  koka_bin_deps=$(get_koka_bin_deps $koka_bin_loc)

  for dep in $koka_bin_deps; do
    for required in $BUNDLE_LIBRARIES; do
      # if dep is in required, then copy it
      if echo $dep | grep -q $required; then
        info "Bundling $dep"
        cp $dep $LIBRARY_DIR/
      fi
    done
  done

  # COLLECT METADATA
  echo "$DISTRO" >"$METADATA_DIR/distro"
  echo "$LIBC_VERSION" >"$METADATA_DIR/libc"

  # BUNDLE EVERYTHING
  status=1
  if [ "$BUILD_MODE" = "stack" ]; then
    stack exec koka -- -e util/bundle -- --metadata="$METADATA_DIR" --solibs="$LIBRARY_DIR" --postfix="temp"
    status=$?
  elif [ "$BUILD_MODE" = "cabal" ]; then
    cabal new-run koka -- -e util/bundle -- --metadata="$METADATA_DIR" --solibs="$LIBRARY_DIR" --postfix="temp"
    status=$?
  fi

  if [ $status -ne 0 ]; then
    stop "Failed to bundle koka"
  fi

  info "Koka bundled"
}

rename_and_export_bundle() {
  info "Renaming and exporting bundle"

  # LOCATE BUNDLE
  bundleloc=$(find ./bundle -name "koka-temp.tar.gz")
  if [ -z "$bundleloc" ]; then
    stop "Failed to find bundle"
  fi

  # GET BUNDLE METADATA
  ARCHITECTURE=$(tar -Oxf "$bundleloc" meta/arch)
  KOKA_VERSION=$(tar -Oxf "$bundleloc" meta/version)

  # If either of these are empty, we have a problem
  if [ -z "$ARCHITECTURE" ] || [ -z "$KOKA_VERSION" ]; then
    stop "Failed to find architecture or version"
  fi

  # MOVE BUNDLE
  new_dir=$()
  new_name="koka-$KOKA_VERSION-$DISTRO-$ARCHITECTURE.tar.gz"
  mv "$bundleloc" "/output/$new_name"
  if [ $? -ne 0 ]; then
    stop "Failed to export bundle"
  fi

  info "Bundle renamed to $new_name, and exported"
}

full_build() {
  info "Starting build"

  mount_overlay

  clean_workdir

  mkdir -p $METADATA_DIR
  mkdir -p $LIBRARY_DIR
  get_koka_version_from_cabal

  build_koka

  bundle_koka

  rename_and_export_bundle

  info "Koka version: $KOKA_VERSION"
  info "Build mode: $BUILD_MODE"
  info "Distro: $DISTRO"
  info "GLIBC Version: $LIBC_VERSION"
  info "Architecture: $ARCHITECTURE"

  info "Build finished"
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

  LIBC_VERSION=$(get_libc_version)

  full_build
}

init_parse_param "$@"
