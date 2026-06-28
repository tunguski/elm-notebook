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
TITLE="elm-notebook" SITECSS="$P/assets/site.css" CSSFILE="$P/src/notebook.css" perl -0pi -e '
  if (index($_, q{name="viewport"}) < 0) {
    s#<meta charset="utf-8">#<meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">#;
  }
  s#<title>.*?</title>#"<title>".$ENV{TITLE}."</title>"#e;
  if (index($_, q{bootstrap-icons}) < 0) {
    s#</head>#<link rel="stylesheet" href="bootstrap-icons-1.11.3.css"></head>#;
  }
  if (index($_, q{id="wsite-css"}) < 0) {
    open(my $f, "<", $ENV{SITECSS}) or die "no site.css: $!";
    local $/; my $css = <$f>; close($f);
    s#</head>#"<style id=\"wsite-css\">".$css."</style></head>"#e;
  }
  if (index($_, q{id="nb-app-css"}) < 0) {
    open(my $f, "<", $ENV{CSSFILE}) or die "no notebook.css: $!";
    local $/; my $css = <$f>; close($f);
    s#</head>#"<style id=\"nb-app-css\">".$css."</style></head>"#e;
  }
' "$HTML"

# Bootstrap Icons are vendored (no CDN); ship the versioned css + woff2 next to the page.
cp "$P/assets/bootstrap-icons-1.11.3.css" "$P/assets/bootstrap-icons-1.11.3.woff2" "$P/assets/logo.svg" "$P/$OUT/"
echo "Done -> $OUT/elm-notebook.html"
