# elm-notebook

A **Jupyter-style notebook for exploring data in real Elm**, in the browser — built on the
[elm-lang](https://github.com/tunguski/elm-lang) compiler.

Live site: **https://tunguski.github.io/elm-notebook/**

A notebook is an ordered list of **cells**. A cell is either *Markdown* (prose) or *Code* — one
Elm expression the **kernel** evaluates. The kernel is stateful: a `name = expr` cell publishes
`name` to every later cell, and `_` is the most recent result. The language is **real Elm**, run
by the vendored [`elm-in-elm` interpreter](vendor/) — so cells have the full `List` / `String` /
`Dict` / `Maybe` libraries, records and `.field` access, `case`, tuples and lambdas. As you go,
the app reads your last result and **suggests concrete next steps**.

```elm
people =
    [ { name = "Ada",   dept = "Eng",    salary = 95 }
    , { name = "Grace", dept = "Eng",    salary = 110 }
    , { name = "Lin",   dept = "Design", salary = 80 }
    ]

List.filter (\r -> r.salary > 90) people     -- a table, rendered as a grid
mean (List.map (\r -> r.salary) people)       -- 92.5
groupBy (\r -> r.dept) people                 -- nested table: one group per dept
```

## Why reuse the interpreter

Rather than ship a toy language, elm-notebook **vendors the real Elm interpreter** that powers
[elm-editor](https://github.com/tunguski/elm-editor) (`Lexer` → `Parser` → `Eval`, written in
Elm). The notebook therefore teaches *genuine, portable Elm*, and gets the whole standard library
for free. The only addition is a tiny **prelude** ([`Notebook.Prelude`](src/Notebook/Prelude.elm))
— `mean`, `groupBy`, `unique` — itself written in plain Elm and loaded as the base scope.

The same vendored [`CodeEditor`](vendor/CodeEditor.elm) + [`Highlight`](vendor/Highlight.elm)
give every cell live syntax highlighting and an auto-growing editor (transparent `<textarea>`
over a highlighted `<pre>`).

## The processing model (mirrors Jupyter)

| Jupyter             | elm-notebook                                           |
| ------------------- | ------------------------------------------------------ |
| Notebook (`.ipynb`) | `Notebook.Doc` — ordered cells + a kernel              |
| Cell (md / code)    | `Notebook.Cell` — `Markdown` or `Code`, with output    |
| Kernel (Python)     | `Notebook.Kernel` — `Lang.Globals` + execution count   |
| `In [n]` / `Out [n]`| execution count threaded per cell                      |
| Rich display        | `Notebook.View` — scalars, records, nested tables, 2-D grids |

**Run all** re-executes the whole notebook from a fresh kernel, and per-cell **Run** re-runs
every cell above first — so outputs are always a reproducible function of the source, with no
hidden out-of-order state.

## Architecture

- **`vendor/`** — the elm-in-elm interpreter, vendored verbatim from elm-editor (`Lang`, `Lexer`,
  `Parser`, `Eval` + `Eval/*` stdlib, `Highlight`, `CodeEditor`). The only change is widening
  `Eval`'s exposing list to surface `evalExpr`.
- **`Notebook.Prelude`** — `mean` / `groupBy` / `unique` in real Elm, the base global scope.
- **`Notebook.Value`** — display & introspection helpers over `Lang.Value` (safe equality,
  table / 2-D detection, formatting).
- **`Notebook.Cell` / `Notebook.Kernel` / `Notebook.Doc`** — the cell, the stateful kernel
  (expression-first detection of bindings), and the document with its editing / running ops.
- **`Notebook.Suggest`** — guided **lessons** and context-aware **next-step suggestions**.
- **`Notebook.View`** — highlighted editors, recursive table rendering (nested tables, headerless
  2-D grids), a small Markdown renderer (with nested lists), and the suggestions panel.
- **`Main`** — the `Browser.element` site.

## Develop

With this repo checked out next to a built [`elm-lang`](https://github.com/tunguski/elm-lang)
(so `../../elm.sh` exists):

```sh
ELM=../../elm.sh ./test.sh     # run the headless test suite (the kernel is pure)
ELM=../../elm.sh ./build.sh    # compile the site → build/elm-notebook.html (self-contained)
```

`.github/workflows/pages.yml` runs the same steps in CI — build the compiler, run the tests
(rendered to a report), compile the site — and deploys to GitHub Pages, gated on the tests
passing.

Part of the [elm-lang](https://github.com/tunguski/elm-lang) ecosystem.
