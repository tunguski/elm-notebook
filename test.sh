#!/usr/bin/env bash
#
# test.sh — run the elm-notebook headless test suite. The kernel runs real Elm through the
# vendored elm-in-elm interpreter (vendor/), and it is pure, so the evaluator + standard
# library, the prelude, the stateful cell kernel, the document model, the value helpers and the
# suggestion/lesson engine are all checked headlessly.
#
# The elm.sh wrapper chdirs to the elm-lang repo root before running, so every path passed to
# the runner must be absolute (computed here after we cd into the script's own dir).
#
#   ELM=../../elm.sh ./test.sh
#
set -euo pipefail
cd "$(dirname "$0")"

ELM="${ELM:-elm}"
P="$(pwd)"

$ELM test "$P/test/NotebookTest.elm" \
  "$P/vendor/Lang.elm" "$P/vendor/Lexer.elm" "$P/vendor/Parser.elm" "$P/vendor/Eval.elm" \
  "$P/vendor/Scale.elm" "$P/vendor/Chart.elm" \
  "$P/src/Notebook/Prelude.elm" "$P/src/Notebook/Value.elm" "$P/src/Notebook/Cell.elm" \
  "$P/src/Notebook/Kernel.elm" "$P/src/Notebook/Doc.elm" "$P/src/Notebook/Suggest.elm" \
  "$P/src/Notebook/Chart.elm" "$P/src/Notebook/Csv.elm" "$P/src/Notebook/Serialize.elm" \
  "$P/vendor/Workspace/Types.elm" "$P/vendor/Workspace/Table.elm" "$P/src/Notebook/Export.elm" \
  "$P/src/Notebook/Deps.elm" "$P/src/Notebook/Hint.elm" "$P/src/Notebook/Profile.elm" "$P/src/Notebook/Import.elm" "$P/src/Notebook/Complete.elm"
