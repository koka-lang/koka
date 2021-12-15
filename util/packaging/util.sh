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

ensure_docker() {
  # Check if docker exists
  if ! has_cmd docker; then
    stop "Docker is required to build the image"
  fi
}

OI=3