#!/bin/bash

# This script allows you easily convert a bundle to an installable package for multiple distros
# But keep in mind the actual binaries in the bundle have to be compatible with the distro

PACKAGE_NAME="koka"
PACKAGE_DESCRIPTION="Koka is a strongly typed functional-style language with effect types and handlers"
PACKAGE_URL="https://koka-lang.github.io/"
PACKAGE_LICENSE="Apache-2.0"

# Dependencies
GENERAL_NIX_DEPENDENCIES="gcc,make,tar,curl,git" # For these programs version doesnt really matter

# (Info)
# Rhel and OpenSuse dependencies dont mind a space next to >= sign
# Debian dependencies need a space next to >= sign
# Alpine dependencies need no space, and use the > sign
# Arch dependencies need no space next to >= sign
RHEL_DEPENDENCIES="glibc >= 2.27"                   # Fedora RedHat CentOS and Rocky
DEBIAN_DEPENDENCIES="libc6 >= 2.27"                 # Ubuntu Debian
ALPINE_DEPENDENCIES="gmp>6,libffi>3.4,musl>1.2"     # Alpine
ARCH_DEPENDENCIES="glibc>=2.33"                     # Arch, Manjaro
OPENSUSE_DEPENDENCIES="glibc >= 2.31"               # OpenSuse

FREEBSD_DEPENDENCIES=""

DARWIN_DEPENDENCIES=""

#---------------------------------------------------------
# Variables
#---------------------------------------------------------

MODE=""
QUIET=""

VERSION=""
ARCHITECTURE=""
BUILD_TARGETS=""

TEMP_DIR=""
EXTRACTED_BUNDLE_DIR="" # $TEMP_DIR/bundle
BUILT_PACKAGE_DIR=""    # $TEMP_DIR/package

CALLER_DIR=""
BUNDLE_LOCATION=""
OUTPUT_DIR=""

#---------------------------------------------------------
# Helper functions
#---------------------------------------------------------

LOG_PREFIX="[KOKA PACKAGER] "
source "$(dirname "$0")/util.sh"

make_extra_temp_dirs() {
  EXTRACTED_BUNDLE_DIR="$TEMP_DIR/bundle"
  mkdir -p "$EXTRACTED_BUNDLE_DIR"

  BUILT_PACKAGE_DIR="$TEMP_DIR/package"
  mkdir -p "$BUILT_PACKAGE_DIR"
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
  debian)
    deps="$DEBIAN_DEPENDENCIES"
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
    deps="$GENERAL_NIX_DEPENDENCIES,$deps"
  fi

  IFS="," read -ra deps_array <<<"$deps"

  deps=""

  # Map to "-d dependeny"
  for dep in "${deps_array[@]}"; do
    deps="$deps -d '$dep'"
  done

  echo "$deps"
}

build_package() {
  TYPE="$1"
  EXT="$2"
  SYSTEM="$3"

  if [ -z "$TYPE" ]; then
    stop "No package type specified"
  fi

  if [ -z "$EXT" ]; then
    stop "No package extension specified"
  fi

  if [ -z "$SYSTEM" ]; then
    stop "No package system specified"
  fi

  dependencies="$(get_dependencies "$SYSTEM")"

  file_name="$PACKAGE_NAME-$VERSION-$SYSTEM-$ARCHITECTURE.$EXT"

  # Remove v from version
  package_version="${VERSION:1}"
  package_iteration=$(semver_to_iteration "$package_version")

  fpm_arguments="-s dir -t '$TYPE' -C '/source' -p '/build/$file_name' $dependencies \
    -n '$PACKAGE_NAME' --description '$PACKAGE_DESCRIPTION' --url '$PACKAGE_URL' --license '$PACKAGE_LICENSE' \
    -v '$package_version' --iteration '$package_iteration' \
    -a native --prefix '/usr/local' \
    --provides '$PACKAGE_NAME' \
    --before-remove /scripts/pre-remove.sh \
    --after-install /scripts/post-install.sh \
    bin/koka=bin/koka lib/koka=lib/ share/koka=share/"

  # Build the package
  docker run -it --rm \
    -v "$EXTRACTED_BUNDLE_DIR:/source:z" \
    -v "$BUILT_PACKAGE_DIR:/build:z" \
    -v "$(pwd)/scripts:/scripts:z" \
    fpm -c "fpm $fpm_arguments"
  # /bin/bash -c "fpm $fpm_arguments"

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
    build_package rpm rpm rhel
    info "RHEL package built successfully"
  fi

  if [[ $packages =~ "debian" ]]; then
    info "Building Debian package"
    build_package deb deb debian
    info "Debian package built successfully"
  fi

  if [[ $packages =~ "alpine" ]]; then
    info "Building Alpine package"
    build_package apk apk alpine
    info "Alpine package built successfully"
  fi

  if [[ $packages =~ "arch" ]]; then
    info "Building Arch package"
    build_package pacman "pkg.tar.zst" arch
    info "Arch package built successfully"
  fi

  if [[ $packages =~ "opensuse" ]]; then
    info "Building OpenSUSE package"
    build_package rpm rpm opensuse
    info "OpenSUSE package built successfully"
  fi

  if [[ $packages =~ "freebsd" ]]; then
    info "Building FreeBSD package"
    build_package freebsd pkg freebsd
    info "FreeBSD package built successfully"
  fi

  if [[ $packages =~ "darwin" ]]; then
    info "Building macOS package"
    build_package darwin pkg darwin
    info "macOS package built successfully"
  fi
}

#---------------------------------------------------------
# Main
#---------------------------------------------------------

build_fpm_docker() {
  info "Building fpm image"
  if [ -n "$QUIET" ]; then
    docker build -q -t fpm -f ./fpm.Dockerfile .
  else
    docker build -t fpm -f ./fpm.Dockerfile .
  fi
}

extract_bundle_to_temp() {
  ensure_tar

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

extract_version_architecture_from_bundle() {
  info "Extracting version from bundle"
  # Extract version from binary using regex magic
  VERSION="$(cat $EXTRACTED_BUNDLE_DIR/meta/version)"
  ARCHITECTURE="$(cat $EXTRACTED_BUNDLE_DIR/meta/arch)"

  if [ -z "$VERSION" ]; then
    stop "Failed to extract version from bundle"
  fi

  if [ -z "$ARCHITECTURE" ]; then
    stop "Failed to extract architecture from bundle"
  fi

  info "Building version $VERSION for $ARCHITECTURE"
}

move_packages() {
  target_location=""
  if [ -z "$OUTPUT_DIR" ]; then
    target_location="$CALLER_DIR/bundle/$VERSION"
  else
    target_location="$OUTPUT_DIR/"
  fi

  mkdir -p "$target_location"

  mv $BUILT_PACKAGE_DIR/* "$target_location"

  info "Packages can be found at $target_location"
}

main_package() {
  info "Starting packaging"
  switch_workdir_to_script
  verify_ran_from_reporoot

  ensure_docker

  # Build a docker image with the necessary tools
  build_fpm_docker

  # Extract the bundle to a temp dir
  auto_temp_dir
  make_extra_temp_dirs

  extract_bundle_to_temp
  extract_version_architecture_from_bundle

  # Build the packages
  info "Building for $BUILD_TARGETS"
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
    -o=* | --output=*)
      OUTPUT_DIR="$flag_arg"
      ;;
    --calldir=*)
      CALLER_DIR="$flag_arg"
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
      BUILD_TARGETS="arch rhel debian alpine opensuse"
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
  info "                           (arch rhel debian alpine opensuse darwin freebsd)"
  info "  -o, --output=<dir>       Specify the output directory"
  info "  -q, --quiet              Suppress output"
  info "  -h, --help               Show this help message"
  info ""
}

main_start() {
  # Make sure we ignore case in string comparisons
  shopt -s nocasematch

  process_options $@
  if [ "$MODE" = "help" ]; then
    main_help
  else
    main_package
  fi
}

main_start "$@"
