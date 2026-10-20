#!/bin/sh
# Assembles the GitHub Pages site (https://koka-lang.github.io/koka/...) from
# documentation builds (util/docs.kk --publish) and playground builds
# (web/playground/dist).
#
#   util/publish-pages.sh place <site> <channel> [latest] [--docs <dir>] [--playground <dir>]
#   util/publish-pages.sh index <site>
#
# Layout, where <channel> is `dev` or a release tag:
#
#   doc/, playground/   the newest stable release, the default that everything links to
#   stable/             the newest stable release
#   dev/                the latest build of the dev branch (earlier ones are not kept)
#   <tag>/              every release tag, e.g. v3.2.9/
#   versions.html       the list of all of them (and versions.json, for a version switcher)
#   index.html          a redirect to doc/
#
# Each channel directory holds doc/ and, for versions that have it, playground/.
# `latest` marks the tag as the newest stable release, which also places it in
# stable/ and at the root. A channel's directories are replaced, except the
# root doc/, which is copied over without deleting so that links to pages a
# release dropped keep working. The doc pages link to each other relatively and
# the playground uses relative paths, so one build serves every location.
#
# The playground's service worker (coi-serviceworker.js) controls every page
# under the directory it is served from, adding cross-origin isolation headers
# that block cross-origin resources the doc pages use, so it must stay inside a
# playground/ directory: never place a playground at the site root.
set -eu

# put <build> <target> [--delete]
put() {
  mkdir -p "$2"
  # source maps double the size of a playground (35 of 90 MB) and are not needed to run it
  rsync -a ${3:-} --exclude '*.map' "$1/" "$2/"
}

place() {
  site=$1; channel=$2; shift 2
  latest=; docs=; playground=
  while [ $# -gt 0 ]; do
    case "$1" in
      latest) latest=1 ;;
      --docs) docs=$2; shift ;;
      --playground) playground=$2; shift ;;
      *) echo "publish-pages: unknown argument $1" >&2; exit 1 ;;
    esac
    shift
  done
  case "$channel" in
    dev|v*) ;;
    *) echo "publish-pages: unknown channel $channel" >&2; exit 1 ;;
  esac
  [ -z "$docs" ] || [ -f "$docs/book.html" ] || { echo "publish-pages: no book.html in $docs" >&2; exit 1; }
  [ -z "$playground" ] || [ -f "$playground/index.html" ] || { echo "publish-pages: no index.html in $playground" >&2; exit 1; }
  targets=$channel
  if [ -n "$latest" ]; then
    targets="$targets stable"
  fi
  for t in $targets; do
    [ -z "$docs" ] || put "$docs" "$site/$t/doc" --delete
    [ -z "$playground" ] || put "$playground" "$site/$t/playground" --delete
  done
  if [ -n "$latest" ]; then
    echo "$channel" > "$site/stable/version.txt"
    [ -z "$docs" ] || put "$docs" "$site/doc"
    [ -z "$playground" ] || put "$playground" "$site/playground" --delete
  fi
}

# links <dir>: the documentation and playground links of a channel directory
links() {
  printf '<a href="%s/doc/index.html">documentation</a>' "$1"
  if [ -f "$site/$1/playground/index.html" ]; then printf ' &middot; <a href="%s/playground/">playground</a>' "$1"; fi
}

# page_start <title>: the start of a generated page
page_start() {
  printf '%s\n' '<!DOCTYPE html>' '<html lang="en"><head><meta charset="utf-8">' \
    '<meta name="viewport" content="width=device-width, initial-scale=1">' \
    "<title>$1</title>" \
    '<style>body{font-family:system-ui,sans-serif;max-width:40em;margin:2em auto;padding:0 1em;line-height:1.7}td{padding-right:1.5em}td.v{font-weight:600}</style>' \
    "</head><body><h1>$1</h1>"
}

index() {
  site=$1
  stable=$(cat "$site/stable/version.txt" 2>/dev/null || true)
  tags=$(cd "$site" && ls -d v*/doc 2>/dev/null | sed 's|/doc$||' | sort -rV)
  # The root leads to the default documentation. Relative, so that a fork's
  # site leads to its own pages; the script keeps the #fragment.
  printf '%s\n' '<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">' \
    '<meta http-equiv="refresh" content="1; url=doc/index.html"><title>Koka</title>' \
    '<script>location.replace("doc/index.html" + location.hash)</script>' \
    '</head><body><a href="doc/index.html">The Koka Programming Language</a></body></html>' > "$site/index.html"
  # A channel directory gets a landing page rather than a redirect: an instant
  # redirect is what phishing pages look like to browsers' safe-browsing checks.
  for c in stable dev $tags; do
    [ -d "$site/$c/doc" ] || continue
    label=$c; [ "$c" = stable ] && label="stable ($stable)"; [ "$c" = dev ] && label="dev (development version)"
    {
      page_start "Koka $label"
      printf '<p>%s</p>\n' "$(links "$c" | sed "s|href=\"$c/|href=\"|g")"
      printf '%s\n' '<p><a href="../versions.html">All versions</a></p></body></html>'
    } > "$site/$c/index.html"
  done
  {
    page_start "Koka versions"
    printf '%s\n' '<table>'
    if [ -n "$stable" ]; then printf '<tr><td class="v">stable</td><td>%s</td><td>%s</td></tr>\n' "$stable" "$(links stable)"; fi
    if [ -d "$site/dev/doc" ]; then printf '<tr><td class="v">dev</td><td>development</td><td>%s</td></tr>\n' "$(links dev)"; fi
    for t in $tags; do printf '<tr><td class="v">%s</td><td></td><td>%s</td></tr>\n' "$t" "$(links "$t")"; done
    printf '%s\n' '</table></body></html>'
  } > "$site/versions.html"
  {
    printf '{"stable": "%s", "versions": [' "$stable"
    sep=
    for c in stable dev $tags; do
      [ -d "$site/$c/doc" ] || continue
      pg=false; [ -f "$site/$c/playground/index.html" ] && pg=true
      printf '%s{"name": "%s", "doc": "%s/doc/", "playground": %s}' "$sep" "$c" "$c" "$pg"
      sep=", "
    done
    printf ']}\n'
  } > "$site/versions.json"
}

cmd=${1:-}; shift || true
case "$cmd" in
  place) place "$@" ;;
  index) index "$@" ;;
  *) echo "usage: publish-pages.sh place <site> <channel> [latest] [--docs <dir>] [--playground <dir>] | index <site>" >&2; exit 1 ;;
esac
