#!/usr/bin/env bash
# check.sh — Run all PLDI 2026 artifact examples and verify their output.
#
# Must be run from the koka repository root:
#   bash test/artifact/pldi26/check.sh
#
# For each example, the script:
#   1. Compiles and runs the example via `stack run koka-plain --fast -- -e`
#   2. Strips compiler noise (load/parse/check/linking/created lines, ANSI
#      escape sequences, and type-warning diagnostic lines)
#   3. Compares the cleaned program output to the pre-saved expected output
#   4. Reports PASS / FAIL and exits non-zero if any example fails

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EXPECTED_DIR="$SCRIPT_DIR/expected"
PASS=0
FAIL=0

# filter_output: strip koka compiler output lines and ANSI escape sequences,
# leaving only the program's own stdout.
#
# Strips:
#   - ANSI/VT100 escape sequences (Isocline terminal probing at startup)
#   - "load    : ...", "parse   : ...", "check   : ...", "linking : ...",
#     "created : ..." compiler phase lines
#   - Type-warning header lines  (path/to/file.kk(line, col): type warning: ...)
#   - Type-warning body lines    (  context :, inferred type:, candidates :,
#                                   hint :, and 15+-space continuation lines)
filter_output() {
    perl -pe '
        s/\e\][^\a\e]*(?:\a|\e\\)//g;  # strip OSC sequences (color queries)
        s/\e\[[0-9;]*[A-Za-z]//g;       # strip CSI sequences (cursor moves etc.)
        s/\e.//g;                        # strip any remaining 2-char ESC sequences
    ' | \
    grep -v -E "^(load|parse|check|linking|created)\s+:" | \
    grep -v -E "^[A-Za-z0-9._/%-]+\.kk\([0-9]+" | \
    grep -v -E "^\s+(context|inferred type|candidates|hint)\s*:" | \
    grep -v -E "^\s{15,}" | \
    awk 'NF || found {found=1; print}'   # drop leading blank lines
}

git pull

check() {
    local name="$1"
    local file="test/artifact/pldi26/examples/$name.kk"
    local expected_file="$EXPECTED_DIR/$name.txt"

    printf "  %-44s " "$name ..."
    local actual
    stack run koka-plain -- -e "$file" # warmup: don't include compilation
    actual=$(stack run koka-plain -- -e "$file" | filter_output)

    local expected_content
    expected_content=$(cat "$expected_file")

    if [ "$actual" = "$expected_content" ]; then
        echo "PASS"
        PASS=$((PASS + 1))
    else
        echo "FAIL"
        echo "    --- expected ---"
        diff <(echo "$expected_content") <(echo "$actual") | head -20 | sed 's/^/    /'
        FAIL=$((FAIL + 1))
    fi
}

echo "Running PLDI 2026 artifact examples..."
echo ""

check intro
check overloading
check comparison
check scope
check grouping
check phantom
check divergence
check busy-beaver
check busy-beaver-higher-order

echo ""
if [ "$FAIL" -eq 0 ]; then
    echo "All $PASS examples passed."
    exit 0
else
    echo "$FAIL of $((PASS + FAIL)) examples FAILED."
    exit 1
fi
