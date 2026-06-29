module Notebook.Filter exposing
    ( Clause, Op(..)
    , ops, opLabel, opFromString
    , blank, matches, apply
    , withCol, withOp, withValue
    )

{-| **Column filters** for a data grid: each [`Clause`](#Clause) tests one column with one operator
(contains / = / ≠ / > / < / ≥ / ≤) against a typed-in value, and [`apply`](#apply) keeps the rows
that pass *every* clause. Pure; [`Notebook.View`](Notebook-View) builds the clauses and renders the
filtered table.

@docs Clause, Op
@docs ops, opLabel, opFromString
@docs blank, matches, apply
@docs withCol, withOp, withValue

-}

import Lang exposing (Value(..))
import Notebook.Value as Value


{-| A comparison operator. -}
type Op
    = Contains
    | Eq
    | Ne
    | Gt
    | Lt
    | Ge
    | Le


{-| One filter clause: a column, an operator and the value to compare against. -}
type alias Clause =
    { col : String, op : Op, value : String }


{-| An empty clause (no column chosen yet — a no-op until configured). -}
blank : Clause
blank =
    { col = "", op = Contains, value = "" }


withCol : String -> Clause -> Clause
withCol name clause =
    { clause | col = name }


withOp : Op -> Clause -> Clause
withOp op clause =
    { clause | op = op }


withValue : String -> Clause -> Clause
withValue value clause =
    { clause | value = value }


{-| Every operator, for a picker. -}
ops : List Op
ops =
    [ Contains, Eq, Ne, Gt, Lt, Ge, Le ]


{-| A short label for an operator. -}
opLabel : Op -> String
opLabel op =
    case op of
        Contains ->
            "contains"

        Eq ->
            "="

        Ne ->
            "≠"

        Gt ->
            ">"

        Lt ->
            "<"

        Ge ->
            "≥"

        Le ->
            "≤"


opFromString : String -> Op
opFromString s =
    case s of
        "=" ->
            Eq

        "≠" ->
            Ne

        ">" ->
            Gt

        "<" ->
            Lt

        "≥" ->
            Ge

        "≤" ->
            Le

        _ ->
            Contains


{-| Keep the rows that satisfy every (configured) clause. -}
apply : List Clause -> List Value -> List Value
apply clauses rows =
    List.filter (\row -> List.all (\c -> rowPasses c row) clauses) rows


rowPasses : Clause -> Value -> Bool
rowPasses clause row =
    if clause.col == "" then
        True

    else
        matches clause row


{-| Does a single row pass a clause? -}
matches : Clause -> Value -> Bool
matches clause row =
    case Value.fieldOf clause.col row of
        Just v ->
            testOp clause.op v clause.value

        Nothing ->
            False


testOp : Op -> Value -> String -> Bool
testOp op v s =
    case op of
        Contains ->
            String.contains (String.toLower s) (String.toLower (Value.displayValue v))

        Eq ->
            Value.displayValue v == s || numCmp v s (\a b -> a == b)

        Ne ->
            not (Value.displayValue v == s || numCmp v s (\a b -> a == b))

        Gt ->
            numCmp v s (\a b -> a > b)

        Lt ->
            numCmp v s (\a b -> a < b)

        Ge ->
            numCmp v s (\a b -> a >= b)

        Le ->
            numCmp v s (\a b -> a <= b)


numCmp : Value -> String -> (Float -> Float -> Bool) -> Bool
numCmp v s cmp =
    case ( asNum v, String.toFloat s ) of
        ( Just a, Just b ) ->
            cmp a b

        _ ->
            False


asNum : Value -> Maybe Float
asNum v =
    case v of
        VNum n ->
            Just n

        _ ->
            Nothing
