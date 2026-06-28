module Notebook.Outline exposing (Heading, headings)

{-| The notebook's **outline**: the Markdown headings (`#`, `##`, …) across its text cells, in order,
each tagged with the cell it lives in so the sidebar can offer jump-to navigation in a long notebook.

@docs Heading, headings

-}

import Notebook.Cell as Cell
import Notebook.Doc exposing (Doc)


{-| One heading: its level (1–6), its text, and the id of the cell it belongs to. -}
type alias Heading =
    { level : Int, text : String, cellId : Int }


{-| Every Markdown heading in the notebook, in document order. -}
headings : Doc -> List Heading
headings doc =
    doc.cells
        |> List.filter Cell.isMarkdown
        |> List.concatMap (\cell -> List.filterMap (lineHeading cell.id) (String.lines cell.source))


lineHeading : Int -> String -> Maybe Heading
lineHeading cellId line =
    let
        trimmed =
            String.trimLeft line

        level =
            countHashes 0 trimmed

        after =
            String.dropLeft level trimmed
    in
    if level >= 1 && level <= 6 && String.startsWith " " after then
        let
            txt =
                String.trim after
        in
        if txt == "" then
            Nothing

        else
            Just { level = level, text = txt, cellId = cellId }

    else
        Nothing


countHashes : Int -> String -> Int
countHashes n s =
    case String.uncons s of
        Just ( '#', rest ) ->
            countHashes (n + 1) rest

        _ ->
            n
