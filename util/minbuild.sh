#!/bin/sh -e

#-----------------------------------------------------------------------
# Minimal build script
# For use on platforms where stack is not working and to document
# the minimal needed commands to build the full compiler.

KOKA_VERSION=2.x.x
KOKA_VARIANT=release

echo ""
echo "------------------------------------------------------------"
echo "WARNING: this is a minimal build script for use on platforms"
echo "         where 'stack' or 'cabal' are not working."
echo "         Use 'stack build' or 'cabal build' when possible"
echo "------------------------------------------------------------"
echo ""

# check for ghc
if ! which ghc > /dev/null ; then
  echo "This build script requires 'ghc'. Install it first, for example:"
  echo "  sudo apt install ghc"
  echo ""
  echo "If ghc is not available as a package you may try 'ghcup' to install it."
  echo "See: <https://www.haskell.org/ghcup> for more information"
  echo ""
  exit 1
fi  

# generate the lexer if not provided 
# note: the Lexer.hs file can be generated on another platform as well.
if ! [ -f src/Syntax/Lexer.hs ] ; then
  if ! which alex > /dev/null ; then
    echo "This build script requires 'alex'. Install it first, for example:"
    echo "  sudo apt install alex"
    echo ""
    exit 1
  fi
  echo "generate lexer.."
  alex src/Syntax/Lexer.x -g -o src/Syntax/Lexer.hs
fi

# create build directory
mkdir -p out/minbuild
set -o xtrace

# build the compiler 
# - add -DDARWIN on macOS, or -DWINDOWS on windows
# - used packages: see 'package.yaml'
ghc -isrc:src/Platform/cpp -odir=out/minbuild -hidir=out/minbuild -o out/minbuild/koka \
    -DKOKA_MAIN=\"koka\" -DKOKA_VARIANT=\"$KOKA_VARIANT\" -DKOKA_VERSION=\"$KOKA_VERSION\" \
    --make -j4 -O2 src/Main.hs src/Platform/cpp/Platform/cconsole.c

set +o xtrace
echo "Koka compiled at: out/minbuild/koka"    
echo ""
