#!/bin/sh
# Compile every self-hosted module (compiler/ and pretty/) as a library against this
# repository's lib/ and kklib/. Fails on any error or warning, and on a module that
# produced no object file.
#
# usage: compiler/check.sh [koka-binary]   (default: the stack build of this checkout)
set -u
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root" || exit 1
koka=${1:-$(stack path --local-install-root)/bin/koka}
[ ! -x "$koka" ] && [ -x "$koka.exe" ] && koka="$koka.exe"
[ -x "$koka" ] || { echo "no compiler at $koka; run 'stack build --fast' first" >&2; exit 1; }
export LANG=C
tag=selfhost-check
rm -rf ".koka/"*"-$tag"
log=$(mktemp "${TMPDIR:-/tmp}/selfhost-check.XXXXXX")
fail=0
count=0
for f in pretty/*.kk $(find compiler -name '*.kk' | sort); do
  count=$((count + 1))
  if ! "$koka" --sharedir=. -i. --buildtag=$tag --library -c "$f" > "$log" 2>&1; then
    echo "FAIL  $f"; cat "$log"; fail=1; continue
  fi
  if grep -q 'warning' "$log"; then
    echo "WARN  $f"; grep -A3 'warning' "$log"; fail=1; continue
  fi
  obj=$(echo "${f%.kk}" | sed 's#/#_#g; s#-#_dash_#g')
  if [ -z "$(find .koka -path "*-$tag/*" \( -name "$obj.o" -o -name "$obj.obj" \))" ]; then
    echo "NOOBJ $f (expected $obj.o or $obj.obj)"; fail=1; continue
  fi
  echo "ok    $f"
done
rm -f "$log"
echo "$count modules checked"
exit $fail
