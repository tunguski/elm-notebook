module Notebook.Value exposing
    ( Value(..)
    , Env
    , Builtin
    , typeName
    , equalValue
    , isTable
    , tableColumns
    , numberToString
    , toDisplayString
    , toInline
    )

{-| The dynamically-typed value a notebook expression evaluates to, and the kernel
environment that maps names to values.

Numbers are all `Float` (there is no separate `Int` — a notebook is for exploring data,
not for type puzzles); a "table" is just a `VList` of `VRecord`s, which the view detects
and renders as a grid. Functions come in two shapes: `VClosure` for user `\x -> …`
lambdas (carrying the environment they captured) and `VBuiltin` for the standard library.

@docs Value, Env, Builtin
@docs typeName, equalValue, isTable, tableColumns
@docs numberToString, toDisplayString, toInline

-}

import Dict exposing (Dict)
import Notebook.Ast exposing (Expr)


{-| A value. `VClosure`/`VBuiltin` carry functions, so never compare two `Value`s with
the built-in `==` (it throws on functions) — use [`equalValue`](#equalValue), which treats
any function as unequal.
-}
type Value
    = VNum Float
    | VStr String
    | VBool Bool
    | VList (List Value)
    | VRecord (List ( String, Value ))
    | VClosure String Expr Env
    | VBuiltin Builtin


{-| A standard-library function. It collects arguments until it has `arity` of them, then
`fn` is run. `args` holds the arguments gathered so far (for partial application).
-}
type alias Builtin =
    { name : String
    , arity : Int
    , args : List Value
    , fn : List Value -> Result String Value
    }


{-| The kernel environment: every name in scope mapped to its value. -}
type alias Env =
    Dict String Value


{-| A short human name for a value's type, used in error messages. -}
typeName : Value -> String
typeName v =
    case v of
        VNum _ ->
            "number"

        VStr _ ->
            "text"

        VBool _ ->
            "bool"

        VList _ ->
            "list"

        VRecord _ ->
            "record"

        VClosure _ _ _ ->
            "function"

        VBuiltin _ ->
            "function"


{-| Structural equality that is safe on values containing functions (functions are never
equal to anything). Used by tests and by the `==` operator in the language.
-}
equalValue : Value -> Value -> Bool
equalValue a b =
    case ( a, b ) of
        ( VNum x, VNum y ) ->
            x == y

        ( VStr x, VStr y ) ->
            x == y

        ( VBool x, VBool y ) ->
            x == y

        ( VList xs, VList ys ) ->
            (List.length xs == List.length ys)
                && List.all identity (List.map2 equalValue xs ys)

        ( VRecord xs, VRecord ys ) ->
            recordEqual xs ys

        _ ->
            False


recordEqual : List ( String, Value ) -> List ( String, Value ) -> Bool
recordEqual xs ys =
    let
        sortFields =
            List.sortBy Tuple.first

        sx =
            sortFields xs

        sy =
            sortFields ys

        sameField ( k1, v1 ) ( k2, v2 ) =
            k1 == k2 && equalValue v1 v2
    in
    (List.length sx == List.length sy)
        && List.all identity (List.map2 sameField sx sy)


{-| Is this value a non-empty list whose every element is a record? Such a value is a
"table" and the view renders it as a grid.
-}
isTable : Value -> Bool
isTable v =
    case v of
        VList ((VRecord _) :: _) ->
            List.all
                (\x ->
                    case x of
                        VRecord _ ->
                            True

                        _ ->
                            False
                )
                (asList v)

        _ ->
            False


asList : Value -> List Value
asList v =
    case v of
        VList xs ->
            xs

        _ ->
            []


{-| The ordered column names of a table: the field names of the first row (rows are
assumed homogeneous, as they are when produced by the table builtins).
-}
tableColumns : Value -> List String
tableColumns v =
    case v of
        VList ((VRecord fields) :: _) ->
            List.map Tuple.first fields

        _ ->
            []


{-| Render a `Float` the way a notebook should: integers without a trailing `.0`, other
numbers trimmed to a sensible precision.
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
        let
            rounded =
                toFloat (round (n * 1.0e6)) / 1.0e6
        in
        String.fromFloat rounded


{-| A one-line, unambiguous rendering of a value (strings quoted) for inline display, the
output gutter and error messages.
-}
toInline : Value -> String
toInline v =
    case v of
        VNum n ->
            numberToString n

        VStr s ->
            "\"" ++ s ++ "\""

        VBool b ->
            if b then
                "True"

            else
                "False"

        VList xs ->
            "[" ++ String.join ", " (List.map toInline xs) ++ "]"

        VRecord fields ->
            "{ "
                ++ String.join ", "
                    (List.map (\( k, val ) -> k ++ " = " ++ toInline val) fields)
                ++ " }"

        VClosure _ _ _ ->
            "<function>"

        VBuiltin b ->
            "<function:" ++ b.name ++ ">"


{-| Like [`toInline`](#toInline) but strings are shown unquoted — used when the value is the
final, headline output of a cell.
-}
toDisplayString : Value -> String
toDisplayString v =
    case v of
        VStr s ->
            s

        _ ->
            toInline v
