module Notebook.Profile exposing (Column, columns)

{-| **Data profiling** for a table value: a quick per-column summary — the column's type, how many
values it has, how many are distinct, and (for numeric columns) its min / max / mean. This is the
"describe the dataframe" overview a data explorer reaches for the moment a table appears.

@docs Column, columns

-}

import Lang exposing (Value(..))
import Notebook.Value as Value
import Set


{-| A one-column summary. `min` / `max` / `mean` are present only for (partly) numeric columns. -}
type alias Column =
    { name : String
    , kind : String
    , count : Int
    , distinct : Int
    , min : Maybe Float
    , max : Maybe Float
    , mean : Maybe Float
    }


{-| Profile every column of a table value (a non-empty list of records); `[]` for anything else. -}
columns : Value -> List Column
columns value =
    case value of
        VList rows ->
            Value.tableColumns value |> List.map (columnStat rows)

        _ ->
            []


columnStat : List Value -> String -> Column
columnStat rows name =
    let
        values =
            List.filterMap (Value.fieldOf name) rows

        nums =
            List.filterMap asNum values

        count =
            List.length values

        distinct =
            values |> List.map Value.inlineValue |> Set.fromList |> Set.size

        kind =
            if count == 0 then
                "empty"

            else if List.length nums == count then
                "number"

            else if List.isEmpty nums then
                "text"

            else
                "mixed"
    in
    { name = name
    , kind = kind
    , count = count
    , distinct = distinct
    , min = List.minimum nums
    , max = List.maximum nums
    , mean =
        if List.isEmpty nums then
            Nothing

        else
            Just (List.sum nums / toFloat (List.length nums))
    }


asNum : Value -> Maybe Float
asNum value =
    case value of
        VNum n ->
            Just n

        _ ->
            Nothing
