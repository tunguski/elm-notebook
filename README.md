# elm-notebook

A **Jupyter-style notebook for exploring data in Elm**, in the browser — written for the
[elm-lang](https://github.com/tunguski/elm-lang) compiler.

Live site: **https://tunguski.github.io/elm-notebook/**

A notebook is an ordered list of **cells**. A cell is either *Markdown* (prose that documents
the analysis) or *Code* — one little Elm-flavoured expression the **kernel** evaluates. Like a
real notebook, the kernel is stateful: a `name = expr` cell publishes `name` to every later
cell, and `_` always refers to the most recent result. As you go, the app reads your last
output and **suggests concrete next steps** — average this column, group by that category,
filter these rows — so you learn the API by running and tweaking real cells.

```elm
people =
  [ { name = "Ada",   dept = "Eng",    salary = 95 }
  , { name = "Grace", dept = "Eng",    salary = 110 }
  , { name = "Lin",   dept = "Design", salary = 80 }
  ]

filter (\row -> row.salary > 90) people     -- a table, rendered as a grid
mean (column "salary" people)               -- 95
groupBy "dept" people                       -- each group with its count and rows
```

## The processing model (mirrors Jupyter)

| Jupyter            | elm-notebook                                        |
| ------------------ | --------------------------------------------------- |
| Notebook (`.ipynb`) | `Notebook.Doc` — ordered cells + a kernel          |
| Cell (md / code)   | `Notebook.Cell` — `Markdown` or `Code`, with output |
| Kernel (Python)    | `Notebook.Kernel` — environment + execution count   |
| `In [n]` / `Out [n]` | execution count threaded per cell                 |
| Rich display       | `Notebook.View` — scalars, lists, tables, errors    |

Every output is a pure, reproducible function of the source: **Run all** re-executes the whole
notebook from a fresh kernel, and the per-cell **Run** re-runs every cell above first — so there
is no hidden out-of-order state, the classic notebook footgun.

## Architecture

The kernel is a small interpreter for a friendly subset of Elm; every module is pure (no DOM
except `View`/`Main`):

- **`Notebook.Ast`** — the expression AST and the cell form (`name = expr` vs a bare expression).
- **`Notebook.Lexer`** — a hand-written tokenizer (`--` line comments included).
- **`Notebook.Parser`** — a precedence-climbing parser: literals, lists, records, field access,
  lambdas, application, the usual operators plus `|>`/`<|`, `if`/`then`/`else`, `let … in`.
- **`Notebook.Value`** — the runtime value (numbers, text, bools, lists, records, functions); a
  *table* is just a list of records. Safe equality and display formatting.
- **`Notebook.Eval`** — the evaluator and the data-processing standard library (~70 functions:
  numbers, lists, `map`/`filter`/`foldl`, strings, records and the table verbs `column`,
  `select`, `groupBy`, `sortByField`, …).
- **`Notebook.Cell` / `Notebook.Kernel` / `Notebook.Doc`** — the cell, the stateful kernel, and
  the notebook document with its editing and running operations.
- **`Notebook.Suggest`** — the teaching layer: guided **lessons** and context-aware **next-step
  suggestions** computed from the last result.
- **`Notebook.View`** — class-styled HTML: editable cells, outputs (incl. table grids), a tiny
  live Markdown renderer, and the suggestions panel.
- **`Main`** — the `Browser.element` site that wires it together.

## Develop

The notebook is compiled by the elm-lang compiler. With this repo checked out next to a built
[`elm-lang`](https://github.com/tunguski/elm-lang) (so `../../elm.sh` exists):

```sh
ELM=../../elm.sh ./test.sh     # run the headless test suite (131 checks, pure kernel)
ELM=../../elm.sh ./build.sh    # compile the site → build/elm-notebook.html (self-contained)
```

`.github/workflows/pages.yml` runs the same steps in CI — build the compiler, run the tests
(rendered to a report), compile the site — and deploys to GitHub Pages, gated on the tests
passing.

## The little language

- **Literals:** numbers `42`, `3.5`; text `"hi"`; `True` / `False`; lists `[1, 2, 3]`; records
  `{ name = "Ada", age = 36 }`.
- **Access & calls:** `record.field`; application by juxtaposition `f x y`; lambdas `\x -> …`.
- **Operators:** `+ - * / ^`, `== /= < > <= >=`, `&& ||`, `++` (join text or lists), and the
  pipes `|>` / `<|`.
- **Forms:** `if c then a else b`; `let a = 1 ; b = 2 in a + b`.
- **Cells:** `name = expr` names a result; a bare `expr` just displays; `_` is the last result.

Part of the [elm-lang](https://github.com/tunguski/elm-lang) ecosystem.
