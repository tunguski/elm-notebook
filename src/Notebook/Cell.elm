module Notebook.Cell exposing
    ( Cell, CellKind(..), Output(..)
    , markdown, code
    , isCode, isMarkdown, hasError
    )

{-| A single notebook cell — the unit a notebook is made of, exactly as in Jupyter.

A cell is either **Markdown** (prose that documents the analysis) or **Code** (one
expression the kernel evaluates). A code cell remembers its last [`Output`](#Output) and its
execution count (`In [n]`), so the view can show `In [3]` / `Out [3]` the way a notebook does.

@docs Cell, CellKind, Output
@docs markdown, code
@docs isCode, isMarkdown, hasError

-}

import Notebook.Value exposing (Value)


{-| The two kinds of cell. -}
type CellKind
    = Markdown
    | Code


{-| The result of running a code cell: nothing yet, a value, or an error message. -}
type Output
    = OutNone
    | OutValue Value
    | OutError String


{-| A cell: a stable `id`, its kind, its source text, its last output and execution count. -}
type alias Cell =
    { id : Int
    , kind : CellKind
    , source : String
    , output : Output
    , count : Maybe Int
    }


{-| A fresh markdown cell. -}
markdown : Int -> String -> Cell
markdown id source =
    { id = id, kind = Markdown, source = source, output = OutNone, count = Nothing }


{-| A fresh, un-run code cell. -}
code : Int -> String -> Cell
code id source =
    { id = id, kind = Code, source = source, output = OutNone, count = Nothing }


{-| Is this a code cell? -}
isCode : Cell -> Bool
isCode cell =
    cell.kind == Code


{-| Is this a markdown cell? -}
isMarkdown : Cell -> Bool
isMarkdown cell =
    cell.kind == Markdown


{-| Did the cell's last run end in an error? -}
hasError : Cell -> Bool
hasError cell =
    case cell.output of
        OutError _ ->
            True

        _ ->
            False
