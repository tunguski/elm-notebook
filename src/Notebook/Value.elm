module Notebook.Value exposing
    ( typeName
    , equalValue
    , isTable
    , tableColumns
    , is2D
    , rows2D
    , numberToString
    , inlineValue
    , displayValue
    , fieldOf
    )

{-| Display and introspection helpers over the interpreter's [`Lang.Value`](Lang#Value).

The kernel evaluates to the real interpreter's value type (numbers, text, bools, chars, lists,
records, tuples, constructors, functions). These helpers are what the view and the suggestion
engine use: structural equality that is safe on functions, "is this a table / a 2-D grid",
column extraction, and one-line / headline string renderings.

@docs typeName, equalValue, isTable, tableColumns, is2D, rows2D
@docs numberToString, inlineValue, displayValue, fieldOf

-}

import Lang exposing (Value(..))


{-| A short human name for a value's type, for error messages and suggestions. -}
typeName : Value -> String
typeName value =
    case value of
        VNum _ ->
            "number"

        VBool _ ->
            "bool"

        VStr _ ->
            "text"

        VChar _ ->
            "char"

        VList _ ->
            "list"

        VRecord _ ->
            "record"

        VTup _ ->
            "tuple"

        VCtor name _ ->
            "constructor " ++ name

        _ ->
            "function"


{-| Structural equality that is safe on values containing functions (a function is never equal
to anything). Used by the tests and any value comparison in the UI.
-}
equalValue : Value -> Value -> Bool
equalValue a b =
    case ( a, b ) of
        ( VNum x, VNum y ) ->
            x == y

        ( VBool x, VBool y ) ->
            x == y

        ( VStr x, VStr y ) ->
            x == y

        ( VChar x, VChar y ) ->
            x == y

        ( VList xs, VList ys ) ->
            listEqual xs ys

        ( VTup xs, VTup ys ) ->
            listEqual xs ys

        ( VCtor n1 xs, VCtor n2 ys ) ->
            n1 == n2 && listEqual xs ys

        ( VRecord xs, VRecord ys ) ->
            recordEqual xs ys

        _ ->
            False


listEqual : List Value -> List Value -> Bool
listEqual xs ys =
    (List.length xs == List.length ys)
        && List.all identity (List.map2 equalValue xs ys)


recordEqual : List ( String, Value ) -> List ( String, Value ) -> Bool
recordEqual xs ys =
    let
        sort =
            List.sortBy Tuple.first

        same ( k1, v1 ) ( k2, v2 ) =
            k1 == k2 && equalValue v1 v2
    in
    (List.length xs == List.length ys)
        && List.all identity (List.map2 same (sort xs) (sort ys))


{-| A non-empty list whose every element is a record — rendered as a grid with a header row. -}
isTable : Value -> Bool
isTable value =
    case value of
        VList ((VRecord _) :: rest) ->
            List.all isRecord rest

        _ ->
            False


isRecord : Value -> Bool
isRecord value =
    case value of
        VRecord _ ->
            True

        _ ->
            False


{-| The column names of a table: the field names of its first row. -}
tableColumns : Value -> List String
tableColumns value =
    case value of
        VList ((VRecord fields) :: _) ->
            List.map Tuple.first fields

        _ ->
            []


{-| A non-empty list whose every element is itself a list (and which is not a table) — a 2-D
array, rendered as a header-less grid.
-}
is2D : Value -> Bool
is2D value =
    case value of
        VList ((VList _) :: rest) ->
            List.all isList rest

        _ ->
            False


isList : Value -> Bool
isList value =
    case value of
        VList _ ->
            True

        _ ->
            False


{-| The rows of a 2-D array. -}
rows2D : Value -> List (List Value)
rows2D value =
    case value of
        VList items ->
            List.map
                (\row ->
                    case row of
                        VList cells ->
                            cells

                        other ->
                            [ other ]
                )
                items

        _ ->
            []


{-| Look up a record field. -}
fieldOf : String -> Value -> Maybe Value
fieldOf name value =
    case value of
        VRecord fields ->
            case List.filter (\( k, _ ) -> k == name) fields of
                ( _, v ) :: _ ->
                    Just v

                [] ->
                    Nothing

        _ ->
            Nothing


{-| Render a `Float` the way a notebook should: integers without a trailing `.0`, others trimmed
to a sensible precision.
-}
numberToString : Float -> String
numberToString n =
    if isNaN n then
        "NaN"

    else if isInfinite n then
        if n < 0 then
            "-Infinity"

        else
            "Infinity"

    else if toFloat (round n) == n && abs n < 1.0e15 then
        String.fromInt (round n)

    else
        String.fromFloat (toFloat (round (n * 1.0e6)) / 1.0e6)


{-| A one-line, unambiguous rendering (strings quoted) for inline display and table cells. -}
inlineValue : Value -> String
inlineValue value =
    case value of
        VNum n ->
            numberToString n

        VBool b ->
            boolText b

        VStr s ->
            "\"" ++ s ++ "\""

        VChar c ->
            "'" ++ String.fromChar c ++ "'"

        VList xs ->
            "[" ++ String.join ", " (List.map inlineValue xs) ++ "]"

        VTup xs ->
            "(" ++ String.join ", " (List.map inlineValue xs) ++ ")"

        VRecord fields ->
            "{ "
                ++ String.join ", " (List.map (\( k, v ) -> k ++ " = " ++ inlineValue v) fields)
                ++ " }"

        VCtor name [] ->
            name

        VCtor name args ->
            name ++ " " ++ String.join " " (List.map inlineAtom args)

        _ ->
            "<function>"


{-| Like [`inlineValue`](#inlineValue) but parenthesises compound constructor arguments. -}
inlineAtom : Value -> String
inlineAtom value =
    case value of
        VCtor _ (_ :: _) ->
            "(" ++ inlineValue value ++ ")"

        _ ->
            inlineValue value


{-| Like [`inlineValue`](#inlineValue) but bare strings are unquoted — for the headline scalar
output of a cell.
-}
displayValue : Value -> String
displayValue value =
    case value of
        VStr s ->
            s

        _ ->
            inlineValue value


boolText : Bool -> String
boolText b =
    if b then
        "True"

    else
        "False"
