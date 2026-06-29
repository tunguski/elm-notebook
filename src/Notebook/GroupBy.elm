module Notebook.GroupBy exposing
    ( Spec, Grid
    , defaultSpec, group
    , withKey, withValue, withAgg
    )

{-| A **group-by aggregation** over a table: bucket the rows by one *key field*, then for each bucket
report the row count and an aggregate (sum / mean / min / max / count) of a *value field*. It is the
one-dimensional cousin of [`Notebook.Pivot`](Notebook-Pivot) — a single grouped summary rather than a
cross-tab — and reuses its [`Agg`](Notebook-Pivot#Agg). Pure; the host supplies the [`Spec`](#Spec)
and renders the returned [`Grid`](#Grid).

@docs Spec, Grid
@docs defaultSpec, group
@docs withKey, withValue, withAgg

-}

import Lang exposing (Value(..))
import Notebook.Pivot as Pivot
import Notebook.Value as Value


{-| What to group: the field to bucket by, the field to aggregate, and how (a [`Pivot.Agg`](Notebook-Pivot#Agg)). -}
type alias Spec =
    { key : String, value : String, agg : Pivot.Agg }


{-| The computed summary: the column headers and one row per distinct key (`[ key, count, aggregate ]`). -}
type alias Grid =
    { columns : List String, rows : List (List String) }


withKey : String -> Spec -> Spec
withKey name spec =
    { spec | key = name }


withValue : String -> Spec -> Spec
withValue name spec =
    { spec | value = name }


withAgg : Pivot.Agg -> Spec -> Spec
withAgg agg spec =
    { spec | agg = agg }


{-| A sensible starting spec: a text column to group by, a numeric column to sum. -}
defaultSpec : Value -> Spec
defaultSpec value =
    let
        cols =
            Value.tableColumns value

        texts =
            List.filter (columnIs isText value) cols

        nums =
            List.filter (columnIs isNum value) cols

        firstOr fallback xs =
            List.head xs |> Maybe.withDefault fallback

        firstCol =
            firstOr "" cols
    in
    { key = firstOr firstCol texts, value = firstOr firstCol nums, agg = Pivot.Sum }


{-| Compute the grouped summary. -}
group : Spec -> Value -> Grid
group spec value =
    let
        records =
            rowsOf value

        keys =
            distinct (List.map (keyOf spec.key) records)

        rowFor k =
            let
                bucket =
                    List.filter (\rec -> keyOf spec.key rec == k) records
            in
            [ k, String.fromInt (List.length bucket), aggregate spec bucket ]
    in
    { columns = [ spec.key, "Count", aggLabelFor spec ], rows = List.map rowFor keys }


aggLabelFor : Spec -> String
aggLabelFor spec =
    Pivot.aggLabel spec.agg ++ " " ++ spec.value


aggregate : Spec -> List Value -> String
aggregate spec records =
    case spec.agg of
        Pivot.Count ->
            String.fromInt (List.length records)

        _ ->
            let
                nums =
                    List.filterMap (numAt spec.value) records
            in
            if List.isEmpty nums then
                ""

            else
                Value.numberToString (combine spec.agg nums)


combine : Pivot.Agg -> List Float -> Float
combine agg nums =
    case agg of
        Pivot.Mean ->
            List.sum nums / toFloat (List.length nums)

        Pivot.Min ->
            List.minimum nums |> Maybe.withDefault 0

        Pivot.Max ->
            List.maximum nums |> Maybe.withDefault 0

        _ ->
            List.sum nums


keyOf : String -> Value -> String
keyOf field record =
    Value.fieldOf field record |> Maybe.map Value.displayValue |> Maybe.withDefault ""


numAt : String -> Value -> Maybe Float
numAt field record =
    case Value.fieldOf field record of
        Just (VNum n) ->
            Just n

        _ ->
            Nothing


rowsOf : Value -> List Value
rowsOf value =
    case value of
        VList items ->
            items

        _ ->
            []


distinct : List String -> List String
distinct xs =
    List.foldl
        (\x seen ->
            if List.member x seen then
                seen

            else
                seen ++ [ x ]
        )
        []
        xs


columnIs : (Value -> Bool) -> Value -> String -> Bool
columnIs pred value name =
    case rowsOf value of
        first :: _ ->
            Value.fieldOf name first |> Maybe.map pred |> Maybe.withDefault False

        [] ->
            False


isNum : Value -> Bool
isNum v =
    case v of
        VNum _ ->
            True

        _ ->
            False


isText : Value -> Bool
isText v =
    case v of
        VStr _ ->
            True

        _ ->
            False
