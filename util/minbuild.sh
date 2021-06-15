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
echo "         where 'stack' is not working."
echo "         Use the 'stack build' command instead when possible"
echo "------------------------------------------------------------"
echo ""

# generate the lexer if not provided
if ! [ -f src/Syntax/Lexer.hsx ] ; then
  if ! which alex > /dev/null ; then
    echo "This build script requires 'alex'. Install it first, for example:"
    echo "  sudo apt install ghc"
    echo ""
    exit 1
  fi
  echo "generate lexer.."
  alex src/Syntax/Lexer.x -g -o src/Syntax/Lexer.hs
fi

# check for ghc
if ! which ghc > /dev/null ; then
  echo "This build script requires 'ghc'. Install it first, for example:"
  echo "  sudo apt install ghc"
  echo ""
  exit 1
fi  

mkdir -p out/build
set -o xtrace

# build the compiler 
# - use -O2 for an optimized version
# - add -DDARWIN on macOS, or -DWINDOWS on windows
# - used packages:  base, containers, directory, process, mtl, haskeline
ghc -isrc:src/Platform/cpp -odir=out/build -hidir=out/build -o out/build/koka \
    -DKOKA_MAIN=\"koka\" -DKOKA_VARIANT=\"$KOKA_VARIANT\" -DKOKA_VERSION=\"$KOKA_VERSION\" \
    --make src/Main.hs src/Platform/cpp/Platform/cconsole.c

set +o xtrace
echo "Koka compiled at: out/build/koka"    
echo ""