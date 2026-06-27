#!/usr/bin/env bash
#
# test.sh — run the elm-notebook headless test suite (pure kernel: lexer, parser,
# evaluator + standard library, the stateful cell kernel, the document model and the
# suggestion/lesson engine).
#
# The elm.sh wrapper chdirs to the elm-lang repo root before running, so every path passed
# to the runner must be absolute (computed here after we cd into the script's own dir).
#
#   ELM=../../elm.sh ./test.sh
#
set -euo pipefail
cd "$(dirname "$0")"

ELM="${ELM:-elm}"
P="$(pwd)"

$ELM test "$P/test/NotebookTest.elm" \
  "$P/src/Notebook/Ast.elm" "$P/src/Notebook/Value.elm" "$P/src/Notebook/Lexer.elm" \
  "$P/src/Notebook/Parser.elm" "$P/src/Notebook/Eval.elm" "$P/src/Notebook/Cell.elm" \
  "$P/src/Notebook/Kernel.elm" "$P/src/Notebook/Doc.elm" "$P/src/Notebook/Suggest.elm"
