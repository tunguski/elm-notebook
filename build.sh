#!/usr/bin/env bash
#
# build.sh — compile the elm-notebook site to a standalone HTML file.
#
# The elm.sh wrapper chdirs to the elm-lang repo root before running, so every path passed
# to `make` must be absolute (computed here after we cd into the script's own dir). Like the
# other elm-lang example apps we compile with --no-check.
#
#   ELM=../../elm.sh ./build.sh
#
set -euo pipefail
cd "$(dirname "$0")"

ELM="${ELM:-elm}"
OUT="build"
P="$(pwd)"

mkdir -p "$OUT"
echo "Compiling elm-notebook with: $ELM"
$ELM make "$P/src/Main.elm" --project="$P/elm.json" -o "$P/$OUT/elm-notebook.html" --no-check

# The compiler owns the output's <head> (charset + title only), so we post-process it: add a
# viewport meta and inline src/notebook.css as a <style> (the app's styling lives there as
# classes; the page stays a single self-contained HTML file). Idempotent on re-runs.
HTML="$P/$OUT/elm-notebook.html"
CSSFILE="$P/src/notebook.css" perl -0pi -e '
  if (index($_, q{name="viewport"}) < 0) {
    s#<meta charset="utf-8">#<meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">#;
  }
  if (index($_, q{bootstrap-icons}) < 0) {
    s#</head>#<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap-icons\@1.11.3/font/bootstrap-icons.css"></head>#;
  }
  if (index($_, q{id="nb-app-css"}) < 0) {
    open(my $f, "<", $ENV{CSSFILE}) or die "no notebook.css: $!";
    local $/; my $css = <$f>; close($f);
    s#</head>#"<style id=\"nb-app-css\">".$css."</style></head>"#e;
  }
' "$HTML"
echo "Done -> $OUT/elm-notebook.html"
