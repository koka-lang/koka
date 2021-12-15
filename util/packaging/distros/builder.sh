#!/bin/sh

# This script is ran from inside the distro specific docker containers
# It automates the building of koka with stack and then cleans everything except the bundles

DISTRO="$1"

if [ -z "$DISTRO" ]; then
  echo "Usage: $0 <distro>"
  exit 1
fi

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

build_koka() {
  echo "Building koka"
  stack build

  echo "Making bundle"
  #stack exec koka -- -e util/bundle -- --postfix="$DISTRO"
  # Bypass bug in koka
  script --return --quiet -c "stack exec koka -- -e util/bundle -- --postfix=\"$DISTRO\"" /dev/null
  if [ $? -ne 0 ]; then
    echo "Failed to build koka"
    exit 1
  fi

  echo "Built koka"
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

mount_overlay

build_koka

export_build