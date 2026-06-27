module Notebook.Serialize exposing (encode, decode)

{-| Persist a notebook as JSON, so it can be autosaved to the browser's local storage and restored
on the next visit. Only the cells (their kind, source, and any input-widget spec) are stored — the
kernel and outputs are recomputed by re-running on load, which keeps the saved form small and always
consistent with the source.

@docs encode, decode

-}

import Json.Decode as D
import Json.Encode as E
import Notebook.Cell exposing (CellKind(..), Control(..), InputSpec)
import Notebook.Doc as Doc exposing (Doc)


{-| Serialise a notebook to a compact JSON string. -}
encode : Doc -> String
encode doc =
    E.encode 0
        (E.object
            [ ( "version", E.int 1 )
            , ( "cells", E.list encodeCell doc.cells )
            ]
        )


encodeCell : Notebook.Cell.Cell -> E.Value
encodeCell cell =
    case cell.kind of
        Markdown ->
            E.object [ ( "kind", E.string "markdown" ), ( "source", E.string cell.source ) ]

        Code ->
            E.object [ ( "kind", E.string "code" ), ( "source", E.string cell.source ) ]

        Input ->
            case cell.input of
                Just spec ->
                    E.object [ ( "kind", E.string "input" ), ( "input", encodeInput spec ) ]

                Nothing ->
                    E.object [ ( "kind", E.string "code" ), ( "source", E.string cell.source ) ]


encodeInput : InputSpec -> E.Value
encodeInput spec =
    E.object
        [ ( "name", E.string spec.name )
        , ( "value", E.string spec.value )
        , ( "control", encodeControl spec.control )
        ]


encodeControl : Control -> E.Value
encodeControl control =
    case control of
        Slider mn mx st ->
            E.object [ ( "t", E.string "slider" ), ( "min", E.float mn ), ( "max", E.float mx ), ( "step", E.float st ) ]

        NumberBox ->
            E.object [ ( "t", E.string "number" ) ]

        TextBox ->
            E.object [ ( "t", E.string "text" ) ]

        Checkbox ->
            E.object [ ( "t", E.string "checkbox" ) ]



-- DECODE ---------------------------------------------------------------------


{-| Rebuild a notebook from its JSON. The result has its cells but an un-run kernel; the caller
runs it. -}
decode : String -> Result String Doc
decode json =
    D.decodeString docDecoder json |> Result.mapError D.errorToString


docDecoder : D.Decoder Doc
docDecoder =
    D.field "cells" (D.list cellDecoder)
        |> D.map (\steps -> List.foldl (\step doc -> step doc) Doc.empty steps)


{-| Each cell decodes to a `Doc -> Doc` that appends it. -}
cellDecoder : D.Decoder (Doc -> Doc)
cellDecoder =
    D.field "kind" D.string
        |> D.andThen
            (\kind ->
                case kind of
                    "markdown" ->
                        D.field "source" D.string |> D.map (Doc.append Markdown)

                    "input" ->
                        D.field "input" inputDecoder |> D.map Doc.appendInput

                    _ ->
                        D.field "source" D.string |> D.map (Doc.append Code)
            )


inputDecoder : D.Decoder InputSpec
inputDecoder =
    D.map3 (\name value control -> { name = name, control = control, value = value })
        (D.field "name" D.string)
        (D.field "value" D.string)
        (D.field "control" controlDecoder)


controlDecoder : D.Decoder Control
controlDecoder =
    D.field "t" D.string
        |> D.andThen
            (\t ->
                case t of
                    "slider" ->
                        D.map3 Slider
                            (D.field "min" D.float)
                            (D.field "max" D.float)
                            (D.field "step" D.float)

                    "text" ->
                        D.succeed TextBox

                    "checkbox" ->
                        D.succeed Checkbox

                    _ ->
                        D.succeed NumberBox
            )
