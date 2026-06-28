module Notebook.Export exposing (valueToTable, cellLinks)

{-| Turn a cell's [`Lang.Value`](Lang#Value) into the workspace's neutral
[`Table`](Workspace-Types#Table), so any step's data can be **exported** to CSV / JSON (and, with a
backend, Excel). A table-shaped value (a list of records) becomes a header + rows; a plain list
becomes a single `value` column; anything else is not exportable.

[`cellLinks`](#cellLinks) renders the in-place download links for a cell (data-URI anchors — the
JS backend has no file-download port, so a `data:` link with a `download` attribute is how a step
is saved).

@docs valueToTable, cellLinks

-}

import Html exposing (Html, a, span, text)
import Html.Attributes as HA
import Lang exposing (Value(..))
import Notebook.Value as Value
import Url
import Workspace.Table as Table
import Workspace.Types exposing (Table)


{-| A cell value as a table, if it has tabular shape. -}
valueToTable : Value -> Maybe Table
valueToTable value =
    if Value.isTable value then
        let
            cols =
                Value.tableColumns value

            rows =
                case value of
                    VList records ->
                        List.map (rowOf cols) records

                    _ ->
                        []
        in
        Just { headers = cols, rows = rows }

    else
        case value of
            VList [] ->
                Nothing

            VList items ->
                Just { headers = [ "value" ], rows = List.map (\v -> [ Value.displayValue v ]) items }

            _ ->
                Nothing


rowOf : List String -> Value -> List String
rowOf cols record =
    List.map
        (\c -> Value.fieldOf c record |> Maybe.map Value.displayValue |> Maybe.withDefault "")
        cols


{-| The CSV / JSON download links for a cell's value (empty if the value is not tabular). `name` is
the base filename. -}
cellLinks : String -> Value -> Html msg
cellLinks name value =
    case valueToTable value of
        Just table ->
            span [ HA.class "nb-export" ]
                [ downloadLink "CSV" (name ++ ".csv") "text/csv" (Table.toCsv table)
                , downloadLink "JSON" (name ++ ".json") "application/json" (Table.toJson table)
                ]

        Nothing ->
            text ""


downloadLink : String -> String -> String -> String -> Html msg
downloadLink labelText filename mime content =
    a
        [ HA.class "nb-dl"
        , HA.href ("data:" ++ mime ++ ";charset=utf-8," ++ Url.percentEncode content)
        , HA.attribute "download" filename
        ]
        [ Html.i [ HA.class "bi bi-download" ] [], text (" " ++ labelText) ]
