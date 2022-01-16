#!/bin/sh

# This script is ran from inside the distro specific docker containers
# It automates the building of koka with stack and then cleans everything except the bundles

DISTRO=""
BUILD_MODE=""

LIBC_VERSION=""
KOKA_VERSION=""
ARCHITECTURE=""

CLEAN_FOLDERS=".koka .stack-work dist dist-newstyle"
CABAL_FLAGS="-O2 --disable-debug-info --enable-executable-stripping --enable-library-stripping"
STACK_FLAGS=""

LOG_PREFIX="[KOKA INTERNAL BUILDER] "

METADATA_DIR="./extra-meta"

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

get_libc_version() {
  libcversion=$(ldd --version 2>&1 | head -n 1)
  libcversion=$(echo $libcversion | awk '{print $NF}')
  echo $libcversion
}

is_libc_musl() {
  libcversion=$(ldd --version 2>&1 | head -n 1)
  if echo $libcversion | grep -q musl; then
    return 0
  else
    return 1
  fi
}

get_koka_version() {
  kk_version=""
  if [ "$BUILD_MODE" = "stack" ]; then
    kk_version=$(stack exec koka -- --version --console=raw | grep "Koka ")
  elif [ "$BUILD_MODE" = "cabal" ]; then
    kk_version=$(cabal new-run koka -- --version --console=raw | grep "Koka ")
  fi

  kk_version="${kk_version%%,*}"   # remove everything after the first ",*"
  kk_version="${kk_version#Koka }" # remove "Koka " prefix
  echo $kk_version
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

build_koka() {
  info "Building koka"

  status=1
  if [ "$BUILD_MODE" = "stack" ]; then
    stack build $STACK_FLAGS
    status=$?
  elif [ "$BUILD_MODE" = "cabal" ]; then
    extra_flags=""

    # You can only link statically on musl, glibc does not support it
    if is_libc_musl; then
      extra_flags="$extra_flags --enable-executable-static"
      echo yes >"$METADATA_DIR/static"
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

  echo "$DISTRO" >"$METADATA_DIR/distro"
  echo "$LIBC_VERSION" >"$METADATA_DIR/libc"

  status=1
  if [ "$BUILD_MODE" = "stack" ]; then
    stack exec koka -- -e util/bundle -- --metadata="$METADATA_DIR" --postfix="temp"
    status=$?
  elif [ "$BUILD_MODE" = "cabal" ]; then
    cabal new-run koka -- -e util/bundle -- --metadata="$METADATA_DIR" --postfix="temp"
    status=$?
  fi

  if [ $status -ne 0 ]; then
    stop "Failed to bundle koka"
  fi

  info "Koka bundled"
}

rename_and_export_bundle() {
  info "Renaming and exporting bundle"

  bundleloc=$(find ./bundle -name "koka-temp.tar.gz")
  if [ -z "$bundleloc" ]; then
    stop "Failed to find bundle"
  fi

  ARCHITECTURE=$(tar -Oxf "$bundleloc" meta/arch)
  KOKA_VERSION=$(tar -Oxf "$bundleloc" meta/version)

  # If either of these are empty, we have a problem
  if [ -z "$ARCHITECTURE" ] || [ -z "$KOKA_VERSION" ]; then
    stop "Failed to find architecture or version"
  fi

  # Get dir from bundleloc
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
