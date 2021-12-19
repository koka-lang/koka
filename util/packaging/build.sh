#!/bin/bash

SUPPORTED_TARGETS="rhel debian arch alpine opensuse"

#---------------------------------------------------------
# Variables
#---------------------------------------------------------

MODE=""
QUIET=""
BUILD_TARGETS=""

#---------------------------------------------------------
# Helper functions
#---------------------------------------------------------

LOG_PREFIX="[KOKA BUILDER] "
source "$(dirname "$0")/util.sh"

#---------------------------------------------------------
# Main
#---------------------------------------------------------

clean_workdir() {
  info "Cleaning up"
  
  rm -rf $CALLER_DIR/.koka
  rm -rf $CALLER_DIR/.stack-work
}

build_docker_images() {
  info "Building docker images"

  quiet_param=""
  if [ -n "$QUIET" ]; then
    quiet_param="-q"
  fi

  # For each target
  for target in $BUILD_TARGETS; do
    info "Building docker image for $target"

    # Build the docker image
    docker build $quiet_param -t koka-$target \
      -f "./$target.Dockerfile" ./distros

    if [ $? -ne 0 ]; then
      stop "Failed to build docker image for $target"
    fi
  done

  info "Docker images built successfully"
}

run_docker_images() {
  info "Compiling os specific packages"

  quiet_param=""
  if [ -n "$QUIET" ]; then
    quiet_param="-q"
  fi

  # For each target
  for target in $BUILD_TARGETS; do
    info "Compiling $target"

    # Build the docker image
    # (Maybe properly fix SELINUX here?)
    docker run $quiet_param --rm \
      --cap-add SYS_ADMIN --security-opt label=disable \
      --tmpfs /tmp/overlay \
      -v "$(pwd)/$KOKA_SOURCE_LOCATION":/code:ro \
      -v "$TEMP_DIR:/output:z" \
      koka-$target

    if [ $? -ne 0 ]; then
      stop "Failed to compile os specific package for $target"
    fi
  done

  info "Compiled os specific packages successfully"
}

move_outputs() {
  info "Moving bundles to output dir"

  mkdir -p "$CALLER_DIR/bundle"
  mv $TEMP_DIR/* "$CALLER_DIR/bundle"

  if [ $? -ne 0 ]; then
    stop "Failed to move output"
  fi

  info "Bundles moved successfully"
}

package_outputs() {
  info "Packaging bundles"

  for target in $BUILD_TARGETS; do
    ./package.sh -o="$CALLER_DIR" -t="$target" "$CALLER_DIR/bundle/koka-$target.tar.gz"

    if [ $? -ne 0 ]; then
      stop "Failed to package $target"
    fi
  done

  info "Bundles packaged successfully"
}

main_build() {
  info "Starting builds"
  switch_workdir_to_script

  clean_workdir

  # If mode is not packageonly
  if [ "$MODE" != "packageonly" ]; then
    ensure_docker

    build_docker_images

    auto_temp_dir

    run_docker_images

    move_outputs
  fi

  # If mode is package or packageonly
  if [ "$MODE" == "package" ] || [ "$MODE" == "packageonly" ]; then
    package_outputs
  fi

  info "Builds finished successfully"
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
    # info "option: $flag, arg: $flag_arg"
    case "$flag" in
    "") break ;;
    -t=* | --targets=*)
      BUILD_TARGETS="$flag_arg"
      ;;
    -p=* | --package=*)
      if [ "$flag_arg" == "yes" ]; then
        MODE="package"
      elif [ "$flag_arg" == "no" ]; then
        MODE="build"
      elif [ "$flag_arg" == "only" ]; then
        MODE="packageonly"
      else
        stop "Invalid package option: $flag_arg"
      fi
      ;;
    -q | --quiet)
      QUIET="yes"
      ;;
    -h | --help | -\? | help | \?)
      MODE="help"
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

  if [ -z "$BUILD_TARGETS" ]; then
    BUILD_TARGETS="$SUPPORTED_TARGETS"
  fi

  # Check if BUILD_TARGETS is in SUPPORTED_TARGETS
  for target in $BUILD_TARGETS; do
    if [ -z "$(echo $SUPPORTED_TARGETS | grep $target)" ]; then
      stop "Invalid target: $target"
    fi
  done
}

main_help() {
  info "command:"
  info "  ./build.sh [options] <bundle file>"
  info ""
  info "options:"
  info "  -t, --targets=<target target>   Specify the targets to build for"
  info "                                  ($SUPPORTED_TARGETS)"
  info "  -p, --package=<yes|no|only>     Package the bundle after building"
  info "  -q, --quiet                     Suppress output"
  info "  -h, --help                      Show this help message"
  info ""
  info "note:"
  info "  This script can only build linux packages right now"
}

main_start() {
  # Make sure we ignore case in string comparisons
  shopt -s nocasematch

  process_options $@
  if [ "$MODE" = "help" ]; then
    main_help
  else
    main_build
  fi
}

main_start "$@"
