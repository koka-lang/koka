#!/bin/sh -e

#-----------------------------------------------------------------------
# Minimal build script
# For use on platforms where stack is not working and to document
# the minimal needed commands to build the full compiler.

KOKA_VERSION=2.3.8
KOKA_VARIANT=release

echo ""
echo "-------------------------------------------------------------"
echo "WARNING: this is a minimal build script for use on platforms"
echo "         where 'stack' or 'cabal' are not working."
echo "         Use 'stack build' or 'cabal new-build' when possible"
echo "-------------------------------------------------------------"
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
# note: the Lexer.hs file can be copied from another platform as well.
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

# extra defs (add -DDARWIN on macOS, or -DWINDOWS on windows)
EXTRADEFS=
case "$(uname)" in
  [Dd]arwin)
    EXTRADEFS="-DDARWIN";;
esac  

# build the compiler (for used packages see 'package.yaml')
set -o xtrace
ghc -isrc:src/Platform/cpp -odir=.koka/minbuild -hidir=.koka/minbuild -o .koka/minbuild/koka \
    -DKOKA_MAIN=\"koka\" -DKOKA_VARIANT=\"$KOKA_VARIANT\" -DKOKA_VERSION=\"$KOKA_VERSION\" $EXTRADEFS \
    --make -j4 -O2 src/Main.hs src/Platform/cpp/Platform/cconsole.c

set +o xtrace
echo "Koka compiled at: out/minbuild/koka"    
echo ""
