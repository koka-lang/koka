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
    echo -e "\033[36m$LOG_PREFIX[I] $@\033[0m"
  fi
}

warn() {
  echo -e "\e[33m$LOG_PREFIX[W] $@\e[0m" >&2
}

stop() {

  echo -e "\e[31m$LOG_PREFIX[E] $@\e[0m" >&2
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

verify_ran_from_reporoot() {
  if [ -z "$CALLER_DIR" ]; then
    stop "There is an error in the script"
  fi

  # Check if ./koka.cabal exists
  if [ ! -f "$CALLER_DIR/koka.cabal" ]; then
    warn "It is recommended you run this script from the repository root"

    # Ask user
    read -p "Do you wish to continue anyway? [y/N] " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
      stop "Exiting"
    fi
  fi
}

#------------------------------------------------------------------------------

semver_to_iteration() {
  semver="$1"
  if [ -z "$semver" ]; then
    stop "No semver specified"
  fi

  # Split by "."
  IFS="." read -ra semver_split <<<"$semver"
  # Pad each to 3 digits
  iteration=""
  for part in "${semver_split[@]}"; do
    iteration="$iteration$(printf "%03d" "$part")"
  done

  echo "$iteration"
}

#------------------------------------------------------------------------------

ensure_tar() {
  if ! has_cmd tar; then
    stop "The tar command is not installed"
  fi
}

# Not necessary because virtualization is userspace
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

has_selinux_and_enabled() {
  if has_cmd getenforce; then
    if [ "$(getenforce)" == "Enforcing" ]; then
      return 0
    fi
  fi

  return 1
}

# ------------------------------------------------------------------------------

# Its so stupid these next couple functions are neccessary

normalize_osarch_koka() {
  arch=$1
  if [ -z "$arch" ]; then
    stop "Architecture is not specified"
  fi

  case "$arch" in
  x86_64* | amd64* | x64*)
    arch="x64"
    ;;
  x86* | i[35678]86* | 386*)
    arch="x86"
    ;;
  arm64* | aarch64* | armv8*)
    arch="arm64"
    ;;
  arm*)
    arch="arm"
    ;;
  esac

  echo $arch
}

# These are not normalized against the koka standard, but against the docker standard
normalize_osarch_docker() {
  arch=$(normalize_osarch_koka $1)

  case "$arch" in
  x64) arch="amd64" ;;
  x86) arch="386" ;;
  arm64) arch="arm64" ;;
  arm) arch="arm" ;;
  esac

  echo $arch
}

# These are normalized against the linux standard
normalize_osarch_linux() {
  arch=$(normalize_osarch_koka $1)

  case "$arch" in
  x64) arch="x86_64" ;;
  x86) arch="i386" ;;
  arm64) arch="aarch64" ;;
  arm) arch="armv7l" ;;
  esac

  echo $arch
}

# ------------------------------------------------------------------------------

docker_flag_exists() {
  subcommand="$1"
  flag="$2"

  if [ -z "$subcommand" ]; then
    stop "docker_flag_exists: subcommand is required"
  fi

  if [ -z "$flag" ]; then
    stop "docker_flag_exists: flag is required"
  fi

  docker_output=$(docker $subcommand --help 2>&1 | grep -i -- "$flag")

  if [ -z "$docker_output" ]; then
    return 1
  else
    return 0
  fi
}

get_docker_architecture() {
  arch=$(docker info | fgrep -i -m 1 "arch: " | awk '{print $2}')

  if [ -z "$arch" ]; then
    arch=$(docker info | fgrep -i -m 1 "architecture: " | awk '{print $2}')
  fi

  if [ -z "$arch" ]; then
    stop "Failed to determine docker architecture"
  fi

  arch=$(normalize_osarch_docker "$arch")

  echo $arch
}

docker_uses_selinux() {
  docker_result=$(docker info | grep -i "selinuxEnabled: true")

  if [ -z "$docker_result" ]; then
    return 1
  else
    return 0
  fi
}

docker_generate_selinux_flags() {
  docker_subcommand="$1"
  if [ -z "$docker_subcommand" ]; then
    stop "No subcommand specified"
  fi

  if has_selinux_and_enabled && docker_uses_selinux; then
    if docker_flag_exists $docker_subcommand "--security-opt"; then
      echo "--security-opt label=disable"
    else
      warn "SELinux is enabled, but Docker does not support SELinux flags"
      warn "This might cause problems, but we will try anyway"
    fi
  fi
}

docker_generate_arch_flags() {
  docker_subcommand="$1"
  if [ -z "$docker_subcommand" ]; then
    stop "No subcommand specified"
  fi
  docker_arch=$2
  if [ -z "$docker_arch" ]; then
    stop "No architecture specified"
  fi

  if docker_flag_exists "$docker_subcommand" "--arch"; then
    echo "--arch $docker_arch"
  elif docker_flag_exists "$docker_subcommand" "--platform"; then
    echo "--platform $docker_arch"
  else
    stop "Docker does not support specifying an architecture"
  fi
}

docker_generate_quiet_flags() {
  docker_subcommand="$1"
  if [ -z "$docker_subcommand" ]; then
    stop "No subcommand specified"
  fi

  if docker_flag_exists "$docker_subcommand" "--quiet"; then
    echo "--quiet"
  fi
}

# ------------------------------------------------------------------------------

test_if_need_docker_multiarch() {
  test_architectures=$1
  if [ -z "$test_architectures" ]; then
    stop "No architectures to test specified"
  fi

  this_arch=$(get_docker_architecture)

  for test_arch in $test_architectures; do
    if [ "$test_arch" != "$this_arch" ]; then
      return 0
    fi
  done

  return 1
}

test_docker_multiarch() {
  test_architectures=$1
  if [ -z "$test_architectures" ]; then
    stop "No architectures to test specified"
  fi

  this_arch=$(get_docker_architecture)

  for test_architecture in $test_architectures; do
    # Skip if the architecture is the same as the current one
    if [ "$this_arch" == "$test_architecture" ]; then
      continue
    fi

    quiet_opt=$(docker_generate_quiet_flags run)
    arch_opt=$(docker_generate_arch_flags run "$test_architecture")
    selinux_opt=$(docker_generate_selinux_flags run)

    # I have no clue why tr -d '\r' is needed, but copilot put it there, and if i remove it it breaks
    test_output=$(docker run $quiet_opt --rm $arch_opt $selinux_opt -t alpine uname -o | tail -n 1 | tr -d '\r')

    if [ "$test_output" != "Linux" ]; then
      return 1
    fi
  done

  return 0
}

install_docker_multiarch() {
  # If not root
  if [ "$(id -u)" != "0" ]; then
    info "To install multiarch, root is needed, sudo will ask for your password now."
  fi

  sudo docker run --rm --privileged multiarch/qemu-user-static --reset -p yes
  if [ $? -ne 0 ]; then
    stop "Failed to install multiarch"
  fi
}

# Supply force to force install
ensure_docker_multiarch() {
  test_architectures=$1
  if [ -z "$test_architectures" ]; then
    stop "No architectures to test specified"
  fi

  if ! test_if_need_docker_multiarch "$test_architectures"; then
    info "No need for multiarch, you are only building native"
    return 0
  fi

  info "Testing if docker supports multiarch"

  test_docker_multiarch "$test_architectures"
  if [ $? -ne 0 ]; then
    info "Multiarch not installed, installing..."

    install_docker_multiarch

    info "Verifying multiarch installation"

    test_docker_multiarch "$test_architectures"
    if [ $? -ne 0 ]; then
      stop "Multiarch failed to install"
    fi
  fi

  info "Multiarch installed successfully"
}
