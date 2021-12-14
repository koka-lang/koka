#!/bin/bash

PACKAGE_NAME="koka"
PACKAGE_DESCRIPTION="Koka is a strongly typed functional-style language with effect types and handlers"
PACKAGE_URL="https://github.com/koka-lang/koka"
PACKAGE_LICENSE="Apache-2.0"

# Dependencies
GENERAL_NIX_DEPENDENCIES="gcc make tar curl cmake git patch patchutils"

RHEL_DEPENDENCIES="ninja-build pkgconf-pkg-config" # Fedora RedHat CentOS and Rocky
UBUNTU_DEPENDENCIES="ninja-build pkgconf"            # Ubuntu Debian
ALPINE_DEPENDENCIES="ninja pkgconf"
ARCH_DEPENDENCIES="ninja pkgconf"
OPENSUSE_DEPENDENCIES="ninja pkgconf"

FREEBSD_DEPENDENCIES="ninja pkgconf"

DARWIN_DEPENDENCIES=""

#---------------------------------------------------------
# Variables
#---------------------------------------------------------

MODE=""
QUIET=""

VERSION=""
BUILD_TARGETS=""

TEMP_DIR=""
EXTRACTED_BUNDLE_DIR="" # $TEMP_DIR/bundle
BUILT_PACKAGE_DIR=""    # $TEMP_DIR/package

CALLER_DIR=""
BUNDLE_LOCATION=""

#---------------------------------------------------------
# Helper functions
#---------------------------------------------------------

info() {
  if [ -z "$QUIET" ]; then
    echo "$@"
  fi
}

warn() {
  echo "$@" >&2
}

stop() {
  warn $@
  exit 1
}

has_cmd() {
  command -v "$1" >/dev/null 2>&1
}

switch_workdir_to_script() {
  CALLER_DIR=$(pwd)
  cd "$(dirname "$0")"
}

get_absolute_path() {
  # If realpath is installed
  if has_cmd realpath; then
    echo $(realpath "$1")
  else
    echo "$(
      cd "$(dirname "$1")"
      pwd
    )/$(basename "$1")"
  fi
}

make_temp_dir() {
  if [ -z "$TEMP_DIR" ]; then
    TEMP_DIR="$(mktemp -d 2>/dev/null || mktemp -d -t koka_packager)"

    EXTRACTED_BUNDLE_DIR="$TEMP_DIR/bundle"
    mkdir -p "$EXTRACTED_BUNDLE_DIR"

    BUILT_PACKAGE_DIR="$TEMP_DIR/package"
    mkdir -p "$BUILT_PACKAGE_DIR"
  fi
}

cleanup_temp_dir() {
  if [ -n "$TEMP_DIR" ]; then
    rm -rf "$TEMP_DIR"
    TEMP_DIR=
  fi
}

#---------------------------------------------------------
# Builds
#---------------------------------------------------------

get_dependencies() {
  system=$1

  deps=""

  case "$system" in
  rhel)
    deps="$RHEL_DEPENDENCIES"
    ;;
  ubuntu)
    deps="$UBUNTU_DEPENDENCIES"
    ;;
  alpine)
    deps="$ALPINE_DEPENDENCIES"
    ;;
  arch)
    deps="$ARCH_DEPENDENCIES"
    ;;
  opensuse)
    deps="$OPENSUSE_DEPENDENCIES"
    ;;
  freebsd)
    deps="$FREEBSD_DEPENDENCIES"
    ;;
  esac

  # Skip extra deps for darwin
  if [ "$system" != "darwin" ]; then
    deps="$GENERAL_NIX_DEPENDENCIES $deps"
  fi

  IFS=" " read -ra deps_array <<<"$deps"

  deps=""

  # Map to "-d dependeny"
  for dep in "${deps_array[@]}"; do
    deps="$deps -d $dep"
  done

  echo "$deps"
}

build_package() {
  TYPE="$1"
  SYSTEM="$2"

  if [ -z "$TYPE" ]; then
    stop "No package type specified"
  fi

  if [ -z "$SYSTEM" ]; then
    stop "No package system specified"
  fi

  dependencies="$(get_dependencies "$SYSTEM")"

  file_name="$PACKAGE_NAME-$VERSION-$SYSTEM.$TYPE"

  docker run --rm -v "$EXTRACTED_BUNDLE_DIR:/source:z" -v "$BUILT_PACKAGE_DIR:/build:z" fpm \
    -s dir -t $TYPE -C "/source" -p "/build/$file_name" $dependencies \
    -n "$PACKAGE_NAME" --description "$PACKAGE_DESCRIPTION" --url "$PACKAGE_URL" --license "$PACKAGE_LICENSE" -v "$VERSION" \
    -a native --prefix "/usr/local" \
    --provides "$PACKAGE_NAME" \
    bin/koka=bin/koka \
    lib/koka=lib \
    share/koka=share

  if [ $? -ne 0 ]; then
    stop "Package build did not return successfully"
  fi

  # Check if rpm has actually been built
  if [ ! -f "$BUILT_PACKAGE_DIR/$file_name" ]; then
    stop "Package build did not create the expected file"
  fi
}

build_packages() {
  packages="$1"

  if [ -z "$packages" ]; then
    stop "No packages specified"
  fi

  if [[ $packages =~ "rhel" ]]; then
    info "Building RHEL package"
    build_package rpm rhel
    info "RHEL package built successfully"
  fi

  if [[ $packages =~ "ubuntu" ]]; then
    info "Building Ubuntu package"
    build_package deb ubuntu
    info "Ubuntu package built successfully"
  fi

  if [[ $packages =~ "alpine" ]]; then
    info "Building Alpine package"
    build_package apk alpine
    info "Alpine package built successfully"
  fi

  if [[ $packages =~ "arch" ]]; then
    info "Building Arch package"
    build_package pacman arch
    info "Arch package built successfully"
  fi

  if [[ $packages =~ "opensuse" ]]; then
    info "Building OpenSUSE package"
    build_package rpm opensuse
    info "OpenSUSE package built successfully"
  fi

  if [[ $packages =~ "freebsd" ]]; then
    info "Building FreeBSD package"
    build_package freebsd freebsd
    info "FreeBSD package built successfully"
  fi

  if [[ $packages =~ "darwin" ]]; then
    info "Building macOS package"
    build_package darwin darwin
    info "macOS package built successfully"
  fi
}

#---------------------------------------------------------
# Main
#---------------------------------------------------------

build_fpm_docker() {
  info "Building fpm image"
  if [ -n "$QUIET" ]; then
    docker build -q -t fpm .
  else
    docker build -t fpm .
  fi
}

extract_bundle_to_temp() {
  # Check if tar is installed
  if ! has_cmd tar; then
    stop "The tar command is not installed"
  fi

  info "Extracting bundle to temp dir"
  tar -xzf "$BUNDLE_LOCATION" -C "$EXTRACTED_BUNDLE_DIR"

  # Check if tar succeeded
  if [ $? -ne 0 ]; then
    stop "Failed to extract bundle to temp dir"
  fi

  # Check if the bundle is valid
  if [ ! -f "$EXTRACTED_BUNDLE_DIR/bin/koka" ]; then
    stop "The bundle is not valid"
  fi
}

extract_version_from_bundle() {
  info "Extracting version from bundle"
  VERSION="$($EXTRACTED_BUNDLE_DIR/bin/koka --version --console=raw)" # get version info
  VERSION="${VERSION%%,*}"                                            # remove everything after the first ",*"
  VERSION="${VERSION#Koka }"                                          # remove "Koka " prefix
  if [ -z "$VERSION" ]; then
    stop "Failed to extract version from bundle"
  fi
  info "Building version $VERSION"
}

move_packages() {
  target_location="$CALLER_DIR/packages/$VERSION/"
  mkdir -p "$target_location"

  mv $BUILT_PACKAGE_DIR/* "$target_location"

  info "Packages can be found at $target_location"
}

main_package() {
  info "Starting packaging"
  switch_workdir_to_script

  # Check if docker exists
  if ! has_cmd docker; then
    stop "Docker is required to build the image"
  fi

  # Build a docker image with the necessary tools
  build_fpm_docker

  # Extract the bundle to a temp dir
  make_temp_dir
  trap cleanup_temp_dir EXIT
  extract_bundle_to_temp
  extract_version_from_bundle

  # Build the packages
  echo "Building for $BUILD_TARGETS"
  build_packages "$BUILD_TARGETS"

  # Move packages to the package dir
  move_packages
}

#---------------------------------------------------------
# Parse arguments
#---------------------------------------------------------

process_options() {
  while :; do
    flag="$1"
    case "$flag" in
    *=*) flag_arg="${flag#*=}" ;;
    *) flag_arg="yes" ;;
    esac
    # echo "option: $flag, arg: $flag_arg"
    case "$flag" in
    "") break ;;
    -q | --quiet)
      QUIET="yes"
      ;;
    -h | --help | -\? | help | \?)
      MODE="help"
      ;;
    -t=* | --targets=*)
      BUILD_TARGETS="$flag_arg"
      ;;
    *) case "$flag" in
      -*) warn "warning: unknown option \"$1\"." ;;
      *) BUNDLE_LOCATION="$1" ;;
      esac ;;
    esac
    shift
  done

  if [ "$MODE" == "help" ]; then
    return
  fi

  if [ -z "$BUNDLE_LOCATION" ]; then
    stop "A bundle has not been specified (--help for more information)"
  else
    BUNDLE_LOCATION="$(get_absolute_path $BUNDLE_LOCATION)"

    # Check if bundle exists and is file
    if [ ! -f "$BUNDLE_LOCATION" ]; then
      stop "Bundle $BUNDLE_LOCATION does not exist"
    fi

    # Check if the bundle is a tar.gz file
    if [ "$(echo $BUNDLE_LOCATION | grep -o '\.tar\.gz$')" != ".tar.gz" ]; then
      stop "Bundle $BUNDLE_LOCATION is not a tar.gz file"
    fi
  fi

  if [ -z "$BUILD_TARGETS" ]; then
    case "$OSTYPE" in
    linux*)
      BUILD_TARGETS="arch rhel ubuntu alpine opensuse"
      ;;
    darwin*)
      BUILD_TARGETS="darwin"
      ;;
    freebsd*)
      BUILD_TARGETS="freebsd"
      ;;
    esac
  fi
}

main_help() {
  info "command:"
  info "  ./package.sh [options] <bundle file>"
  info ""
  info "options:"
  info "  -t, --targets=<url>      Specify the targets to build for"
  info "                           (arch rhel ubuntu alpine opensuse darwin freebsd)"
  info "  -q, --quiet              suppress output"
  info "  -h, --help               show this help message"
  info ""
}

main_start() {
  shopt -s nocasematch

  process_options $@
  if [ "$MODE" = "help" ]; then
    main_help
  else
    main_package
  fi
}

main_start "$@"
