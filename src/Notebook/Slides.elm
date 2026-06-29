module Notebook.Slides exposing (Slide, slides)

{-| The notebook seen as a **slide deck** for presentation mode. A new slide begins at every
top-level (`#` or `##`) Markdown heading; the cells between headings travel with their heading. So a
well-structured notebook — headings introducing each step — reads straight through as a talk, with
its code and outputs shown live on each slide.

@docs Slide, slides

-}

import Notebook.Cell as Cell exposing (Cell)
import Notebook.Doc exposing (Doc)


{-| One slide: its heading title (empty for a leading run with no heading) and the cells on it. -}
type alias Slide =
    { title : String, cells : List Cell }


{-| Split a notebook into slides at its top-level headings, in document order. -}
slides : Doc -> List Slide
slides doc =
    List.foldl place [] doc.cells
        |> List.reverse
        |> List.map reverseCells


place : Cell -> List Slide -> List Slide
place cell acc =
    case heading cell of
        Just title ->
            { title = title, cells = [ cell ] } :: acc

        Nothing ->
            case acc of
                slide :: rest ->
                    { slide | cells = cell :: slide.cells } :: rest

                [] ->
                    [ { title = "", cells = [ cell ] } ]


reverseCells : Slide -> Slide
reverseCells slide =
    { slide | cells = List.reverse slide.cells }


{-| The slide title a Markdown cell starts (a level-1 or -2 heading on its first non-blank line). -}
heading : Cell -> Maybe String
heading cell =
    if Cell.isMarkdown cell then
        firstHeading (String.lines cell.source)

    else
        Nothing


firstHeading : List String -> Maybe String
firstHeading lines =
    case lines of
        line :: rest ->
            let
                trimmed =
                    String.trimLeft line
            in
            if String.startsWith "## " trimmed then
                Just (String.trim (String.dropLeft 3 trimmed))

            else if String.startsWith "# " trimmed then
                Just (String.trim (String.dropLeft 2 trimmed))

            else if String.trim line == "" then
                firstHeading rest

            else
                Nothing

        [] ->
            Nothing
