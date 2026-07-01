module Notebook.Serialize exposing (encode, decode, encodeDoc, decoder)

{-| Persist a notebook as JSON, so it can be autosaved to the browser's local storage and restored
on the next visit. Only the cells (their kind, source, and any input-widget spec) are stored — the
kernel and outputs are recomputed by re-running on load, which keeps the saved form small and always
consistent with the source.

@docs encode, decode

-}

import Json.Decode as D
import Json.Encode as E
import Notebook.Cell as Cell exposing (Cell, CellKind(..), Control(..), InputSpec)
import Notebook.Doc as Doc exposing (Doc)
import Workspace.Serialize as WSerialize
import Workspace.Types exposing (DocRef)


{-| Serialise a notebook to a compact JSON string. -}
encode : Doc -> String
encode doc =
    E.encode 0 (encodeDoc doc)


{-| Serialise a notebook to a JSON value (used when a notebook is the inner document of a
[workspace](Workspace) `Stored` record). -}
encodeDoc : Doc -> E.Value
encodeDoc doc =
    E.object
        [ ( "version", E.int 2 )
        , ( "cells", E.list encodeCell doc.cells )
        , ( "refs", WSerialize.encodeRefs doc.refs )
        ]


encodeCell : Cell -> E.Value
encodeCell cell =
    -- The stable id is stored so a cross-document reference to "step N" survives reopening.
    case cell.kind of
        Markdown ->
            E.object [ ( "id", E.int cell.id ), ( "kind", E.string "markdown" ), ( "source", E.string cell.source ) ]

        Code ->
            E.object [ ( "id", E.int cell.id ), ( "kind", E.string "code" ), ( "source", E.string cell.source ) ]

        Input ->
            case cell.input of
                Just spec ->
                    E.object [ ( "id", E.int cell.id ), ( "kind", E.string "input" ), ( "input", encodeInput spec ) ]

                Nothing ->
                    E.object [ ( "id", E.int cell.id ), ( "kind", E.string "code" ), ( "source", E.string cell.source ) ]


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


{-| The notebook-document decoder, for use as a [workspace](Workspace) `DocCodec`. -}
decoder : D.Decoder Doc
decoder =
    docDecoder


docDecoder : D.Decoder Doc
docDecoder =
    D.map2 buildDoc
        (D.field "cells" (D.list cellDecoder))
        (D.oneOf [ D.field "refs" WSerialize.refsDecoder, D.succeed [] ])


{-| Assemble cells, restoring each cell's stored id. Legacy notebooks (v1) have no ids, so any cell
missing one is assigned a fresh id past the largest stored id — keeping every id unique and stable
from here on. -}
buildDoc : List ( Maybe Int, Int -> Cell ) -> List DocRef -> Doc
buildDoc raw refs =
    let
        maxStored =
            raw |> List.filterMap Tuple.first |> List.maximum |> Maybe.withDefault 0

        assign ( maybeId, mk ) ( nextFree, acc ) =
            case maybeId of
                Just id ->
                    ( nextFree, mk id :: acc )

                Nothing ->
                    ( nextFree + 1, mk nextFree :: acc )

        ( _, reversed ) =
            List.foldl assign ( maxStored + 1, [] ) raw
    in
    Doc.fromCells (List.reverse reversed) refs


{-| Each cell decodes to its stored id (if any) and a builder that stamps a final id onto it. -}
cellDecoder : D.Decoder ( Maybe Int, Int -> Cell )
cellDecoder =
    D.map2 Tuple.pair
        (D.maybe (D.field "id" D.int))
        (D.field "kind" D.string
            |> D.andThen
                (\kind ->
                    case kind of
                        "markdown" ->
                            D.field "source" D.string |> D.map (\s id -> Cell.markdown id s)

                        "input" ->
                            D.field "input" inputDecoder |> D.map (\spec id -> Cell.inputCell id spec)

                        _ ->
                            D.field "source" D.string |> D.map (\s id -> Cell.code id s)
                )
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
