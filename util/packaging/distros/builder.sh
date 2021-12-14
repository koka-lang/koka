#!/bin/sh

# This script is ran from inside the distro specific docker containers
# It automates the building of koka with stack and then cleans everything except the bundles

DISTRO="$1"

if [ -z "$DISTRO" ]; then
  echo "Usage: $0 <distro>"
  exit 1
fi

echo "Building koka"
stack build

echo "Making bundle"
stack exec koka -- -e util/bundle -- --postfix="$DISTRO"

echo "Cleaning up"
stack clean
rm -r .koka
rm -r .stack-work
echo "Done"