module Notebook.Overview exposing (Stats, of_)

{-| A small **overview** of a notebook for the sidebar: how many cells (and of which kind), how many
variables it defines, how many cells are in error, and a rough word count / reading time of its prose.
Pure; [`Notebook.View`](Notebook-View) renders the [`Stats`](#Stats).

@docs Stats, of_

-}

import Notebook.Cell as Cell exposing (Cell)
import Notebook.Doc as Doc exposing (Doc)


{-| The headline numbers about a notebook. -}
type alias Stats =
    { cells : Int
    , code : Int
    , text : Int
    , input : Int
    , variables : Int
    , errors : Int
    , words : Int
    , readMins : Int
    }


{-| Summarise a notebook. -}
of_ : Doc -> Stats
of_ doc =
    let
        cells =
            doc.cells

        total =
            List.length cells

        codeN =
            List.length (List.filter Cell.isCode cells)

        textN =
            List.length (List.filter Cell.isMarkdown cells)

        words =
            List.sum (List.map cellWords (List.filter Cell.isMarkdown cells))
    in
    { cells = total
    , code = codeN
    , text = textN
    , input = total - codeN - textN
    , variables = List.length (Doc.variables doc)
    , errors = List.length (List.filter Cell.hasError cells)
    , words = words
    , readMins = Basics.max 1 ((words + 199) // 200)
    }


cellWords : Cell -> Int
cellWords cell =
    List.length (String.words cell.source)
