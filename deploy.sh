#!/usr/bin/env zsh
set -euo pipefail

UPSTREAM_REPO="TimWhiting/koka"
FORK_REPO="TimWhiting/koka"
BRANCH="feature/web-playground"

RUN_ID=$(gh run list -R "$UPSTREAM_REPO" --workflow playground.yml --branch "$BRANCH" --json databaseId --jq '.[0].databaseId')

if [[ -z "${RUN_ID:-}" || "$RUN_ID" == "null" ]]; then
echo "No runs found for $UPSTREAM_REPO on $BRANCH"
exit 1
fi

echo "Using run: $RUN_ID"

TMP_ROOT=$(mktemp -d /tmp/koka-pages-XXXXXX)
ART_DIR="$TMP_ROOT/artifact"
SITE_DIR="$TMP_ROOT/site"
PUB_DIR="$TMP_ROOT/publish"

mkdir -p "$ART_DIR" "$SITE_DIR" "$PUB_DIR"

gh run download "$RUN_ID" -R "$UPSTREAM_REPO" -n github-pages -D "$ART_DIR"
tar -xf "$ART_DIR/artifact.tar" -C "$SITE_DIR"

cd "$PUB_DIR"
git init
git checkout -b gh-pages
cp -R "$SITE_DIR"/. .
touch .nojekyll
git add -A
git commit -m "Deploy Pages from run $RUN_ID"

git remote add origin "https://github.com/$FORK_REPO.git"
git push -f origin gh-pages

gh api -X PUT "repos/$FORK_REPO/pages" --input - <<'JSON'
{ "source": { "branch": "gh-pages", "path": "/"}}
JSON

echo "Published to https://${FORK_REPO%%/*}.github.io/${FORK_REPO##*/}/"