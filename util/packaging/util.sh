#!/bin/bash

# Useful functions for the other scripts

# Relative to this directory
KOKA_SOURCE_LOCATION="../../"

# Variables
TEMP_DIR=""
QUIET=""
CALLER_DIR=""

if ! [[ "${BASH_SOURCE[0]}" != "${0}" ]]; then
  echo "This script should be sourced, not run"
  exit 1
fi

info() {
  if [ -z "$QUIET" ]; then
    echo "$LOG_PREFIX$@"
  fi
}

warn() {
  echo "$LOG_PREFIX$@" >&2
}

stop() {
  warn $@
  exit 1
}

has_cmd() {
  command -v "$1" >/dev/null 2>&1
}

switch_workdir_to_script() {
  if [ -z "$CALLER_DIR" ]; then
    CALLER_DIR=$(pwd)
  fi

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
  fi
}

cleanup_temp_dir() {
  if [ -n "$TEMP_DIR" ]; then
    rm -rf "$TEMP_DIR"
    TEMP_DIR=
  fi
}

auto_temp_dir() {
  make_temp_dir
  trap cleanup_temp_dir EXIT
}

#------------------------------------------------------------------------------

ensure_tar() {
  if ! has_cmd tar; then
    stop "The tar command is not installed"
  fi
}

ensure_kvm() {
  virtualization=$(lscpu | grep -i "virtualization" | awk '{print $2}')

  if [ "$virtualization" != "AMD-V" ] && [ "$virtualization" != "VT-x" ]; then
    stop "CPU does not support virtualization, or is not enabled"
  fi

  if [ ! -c /dev/kvm ]; then
    stop "KVM not found or enabled"
  fi
}

ensure_docker() {
  # Check if docker exists
  if ! has_cmd docker; then
    stop "Docker is required to build the image"
  fi
}

test_docker_multiarch() {
  _test_architectures=$1
  if [ -z "$_test_architectures" ]; then
    stop "No architectures to test specified"
  fi

  this_arch=$(docker info | fgrep -i -m 1 "arch: " | awk '{print $2}')

  if [ -z "$this_arch" ]; then
    this_arch=$(docker info | fgrep -i -m 1 "architecture: " | awk '{print $2}')
  fi

  if [ -z "$this_arch" ]; then
    stop "Failed to determine docker architecture"
  fi

  for _test_architecture in $_test_architectures; do
    # Skip if the architecture is the same as the current one
    if [ "$this_arch" == "$_test_architecture" ]; then
      continue
    fi

    # I have no clue why tr -d '\r' is needed, but copilot put it there, and if i remove it it breaks
    test_output=$(docker run --rm --arch $_test_architecture --security-opt label=disable -t alpine uname -o | tail -n 1 | tr -d '\r')

    if [ "$test_output" != "Linux" ]; then
      return 1
    fi
  done

  return 0
}

ensure_docker_multiarch() {
  _test_architectures=$1
  if [ -z "$_test_architectures" ]; then
    stop "No architectures to test specified"
  fi

  test_docker_multiarch "$_test_architectures"
  if [ $? -ne 0 ]; then
    info "Multiarch not installed, installing..."

    # If not root
    if [ "$(id -u)" != "0" ]; then
      info "To install multiarch, root is needed, sudo will ask for your password now."
    fi

    sudo docker run --rm --privileged multiarch/qemu-user-static --reset -p yes
    if [ $? -ne 0 ]; then
      stop "Failed to install multiarch"
    fi
  fi

  test_docker_multiarch "$_test_architectures"
  if [ $? -ne 0 ]; then
    stop "Multiarch failed to install"
  fi
}
