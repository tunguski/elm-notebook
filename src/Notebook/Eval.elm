module Notebook.Eval exposing (eval, apply, evalString, defaultEnv, builtinNames)

{-| The notebook evaluator and its standard library.

[`eval`](#eval) walks an [`Ast.Expr`](Notebook-Ast#Expr) in an environment and produces a
[`Value`](Notebook-Value#Value) (or an error message). [`apply`](#apply) is how a value is
called — closures extend their captured environment, builtins gather arguments until they
have enough. [`defaultEnv`](#defaultEnv) is the standard library every cell starts with: a
focused set of data-processing functions (numbers, lists, strings, records and tables).

@docs eval, apply, evalString, defaultEnv, builtinNames

-}

import Dict exposing (Dict)
import Notebook.Ast exposing (Expr(..))
import Notebook.Parser as Parser
import Notebook.Value as Value exposing (Builtin, Env, Value(..))


{-| Parse and evaluate a source string in an environment. -}
evalString : Env -> String -> Result String Value
evalString env src =
    Parser.parse src |> Result.andThen (eval env)


{-| Evaluate an expression. -}
eval : Env -> Expr -> Result String Value
eval env expr =
    case expr of
        ENum n ->
            Ok (VNum n)

        EStr s ->
            Ok (VStr s)

        EBool b ->
            Ok (VBool b)

        EList items ->
            evalAll env items |> Result.map VList

        ERecord fields ->
            evalFields env fields |> Result.map VRecord

        EVar name ->
            case Dict.get name env of
                Just v ->
                    Ok v

                Nothing ->
                    Err ("undefined name: `" ++ name ++ "`")

        EField recExpr field ->
            eval env recExpr |> Result.andThen (getField field)

        ELambda param body ->
            Ok (VClosure param body env)

        EApply fnExpr argExpr ->
            eval env fnExpr
                |> Result.andThen
                    (\fn ->
                        eval env argExpr
                            |> Result.andThen (\arg -> apply fn arg)
                    )

        ENeg inner ->
            eval env inner
                |> Result.andThen
                    (\v ->
                        case v of
                            VNum n ->
                                Ok (VNum (negate n))

                            _ ->
                                Err ("cannot negate a " ++ Value.typeName v)
                    )

        EBinop op left right ->
            evalBinop env op left right

        EIf cond thenE elseE ->
            eval env cond
                |> Result.andThen
                    (\c ->
                        case c of
                            VBool True ->
                                eval env thenE

                            VBool False ->
                                eval env elseE

                            _ ->
                                Err ("`if` needs a bool condition, got a " ++ Value.typeName c)
                    )

        ELet binds body ->
            evalLet env binds body


evalAll : Env -> List Expr -> Result String (List Value)
evalAll env exprs =
    List.foldr
        (\e acc -> Result.map2 (::) (eval env e) acc)
        (Ok [])
        exprs


evalFields : Env -> List ( String, Expr ) -> Result String (List ( String, Value ))
evalFields env fields =
    List.foldr
        (\( k, e ) acc -> Result.map2 (\v rest -> ( k, v ) :: rest) (eval env e) acc)
        (Ok [])
        fields


evalLet : Env -> List ( String, Expr ) -> Expr -> Result String Value
evalLet env binds body =
    case binds of
        [] ->
            eval env body

        ( name, valueExpr ) :: rest ->
            eval env valueExpr
                |> Result.andThen
                    (\v -> evalLet (Dict.insert name v env) rest body)


getField : String -> Value -> Result String Value
getField field v =
    case v of
        VRecord fields ->
            case lookup field fields of
                Just found ->
                    Ok found

                Nothing ->
                    Err ("record has no field `" ++ field ++ "`")

        _ ->
            Err ("cannot access `." ++ field ++ "` on a " ++ Value.typeName v)


lookup : String -> List ( String, a ) -> Maybe a
lookup key pairs =
    case pairs of
        ( k, v ) :: rest ->
            if k == key then
                Just v

            else
                lookup key rest

        [] ->
            Nothing



-- APPLICATION ----------------------------------------------------------------


{-| Apply a function value to one argument. Closures evaluate their body in the captured
environment; builtins accumulate arguments and fire once they have their full arity.
-}
apply : Value -> Value -> Result String Value
apply fn arg =
    case fn of
        VClosure param body captured ->
            eval (Dict.insert param arg captured) body

        VBuiltin b ->
            let
                gathered =
                    b.args ++ [ arg ]
            in
            if List.length gathered >= b.arity then
                b.fn gathered

            else
                Ok (VBuiltin { b | args = gathered })

        _ ->
            Err ("not a function: " ++ Value.toInline fn ++ " (a " ++ Value.typeName fn ++ ")")



-- OPERATORS ------------------------------------------------------------------


evalBinop : Env -> String -> Expr -> Expr -> Result String Value
evalBinop env op left right =
    case op of
        "&&" ->
            eval env left
                |> Result.andThen
                    (\l ->
                        case l of
                            VBool False ->
                                Ok (VBool False)

                            VBool True ->
                                eval env right |> Result.andThen asBoolValue

                            _ ->
                                Err "`&&` needs bools"
                    )

        "||" ->
            eval env left
                |> Result.andThen
                    (\l ->
                        case l of
                            VBool True ->
                                Ok (VBool True)

                            VBool False ->
                                eval env right |> Result.andThen asBoolValue

                            _ ->
                                Err "`||` needs bools"
                    )

        _ ->
            eval env left
                |> Result.andThen
                    (\l ->
                        eval env right
                            |> Result.andThen (\r -> applyBinop op l r)
                    )


applyBinop : String -> Value -> Value -> Result String Value
applyBinop op l r =
    case op of
        "+" ->
            arith (+) l r

        "-" ->
            arith (-) l r

        "*" ->
            arith (*) l r

        "/" ->
            arith (/) l r

        "^" ->
            arith (^) l r

        "==" ->
            Ok (VBool (Value.equalValue l r))

        "/=" ->
            Ok (VBool (not (Value.equalValue l r)))

        "<" ->
            order (\o -> o == LT) l r

        ">" ->
            order (\o -> o == GT) l r

        "<=" ->
            order (\o -> o /= GT) l r

        ">=" ->
            order (\o -> o /= LT) l r

        "++" ->
            concatOp l r

        _ ->
            Err ("unknown operator `" ++ op ++ "`")


arith : (Float -> Float -> Float) -> Value -> Value -> Result String Value
arith f l r =
    Result.map2 (\a b -> VNum (f a b)) (asNum l) (asNum r)


order : (Order -> Bool) -> Value -> Value -> Result String Value
order test l r =
    compareValues l r |> Result.map (\o -> VBool (test o))


compareValues : Value -> Value -> Result String Order
compareValues l r =
    case ( l, r ) of
        ( VNum a, VNum b ) ->
            Ok (compare a b)

        ( VStr a, VStr b ) ->
            Ok (compare a b)

        ( VBool a, VBool b ) ->
            Ok (compare (boolRank a) (boolRank b))

        _ ->
            Err ("cannot compare a " ++ Value.typeName l ++ " with a " ++ Value.typeName r)


boolRank : Bool -> Int
boolRank b =
    if b then
        1

    else
        0


concatOp : Value -> Value -> Result String Value
concatOp l r =
    case ( l, r ) of
        ( VStr a, VStr b ) ->
            Ok (VStr (a ++ b))

        ( VList a, VList b ) ->
            Ok (VList (a ++ b))

        _ ->
            Err "`++` joins two texts or two lists"


asBoolValue : Value -> Result String Value
asBoolValue v =
    case v of
        VBool _ ->
            Ok v

        _ ->
            Err ("expected a bool, got a " ++ Value.typeName v)



-- EXTRACTORS -----------------------------------------------------------------


asNum : Value -> Result String Float
asNum v =
    case v of
        VNum n ->
            Ok n

        _ ->
            Err ("expected a number, got a " ++ Value.typeName v ++ " (" ++ Value.toInline v ++ ")")


asStr : Value -> Result String String
asStr v =
    case v of
        VStr s ->
            Ok s

        _ ->
            Err ("expected text, got a " ++ Value.typeName v)


asList : Value -> Result String (List Value)
asList v =
    case v of
        VList xs ->
            Ok xs

        _ ->
            Err ("expected a list, got a " ++ Value.typeName v)


asRecord : Value -> Result String (List ( String, Value ))
asRecord v =
    case v of
        VRecord fs ->
            Ok fs

        _ ->
            Err ("expected a record, got a " ++ Value.typeName v)


asInt : Value -> Result String Int
asInt v =
    asNum v |> Result.map round



-- STANDARD LIBRARY -----------------------------------------------------------


{-| The names every notebook starts with, for autocomplete / docs. -}
builtinNames : List String
builtinNames =
    Dict.keys defaultEnv


{-| The default kernel environment: the standard library. -}
defaultEnv : Env
defaultEnv =
    Dict.fromList (constants ++ stdlib)


constants : List ( String, Value )
constants =
    [ ( "pi", VNum pi )
    , ( "e", VNum e )
    ]


stdlib : List ( String, Value )
stdlib =
    [ -- numbers
      b1 "abs" (mapNum abs)
    , b1 "negate" (mapNum negate)
    , b1 "sqrt" (mapNum sqrt)
    , b1 "round" (mapNum (\n -> toFloat (round n)))
    , b1 "floor" (mapNum (\n -> toFloat (floor n)))
    , b1 "ceiling" (mapNum (\n -> toFloat (ceiling n)))
    , b1 "not" notFn
    , b2 "min" (num2 Basics.min)
    , b2 "max" (num2 Basics.max)
    , b2 "mod" modFn
    , b3 "clamp" clampFn

    -- lists
    , b2 "range" rangeFn
    , b1 "length" lengthFn
    , b1 "sum" (numFold (+) 0)
    , b1 "product" (numFold (*) 1)
    , b1 "mean" meanFn
    , b1 "median" medianFn
    , b1 "maximum" (numReduce Basics.max "maximum")
    , b1 "minimum" (numReduce Basics.min "minimum")
    , b1 "stddev" stddevFn
    , b1 "head" headFn
    , b1 "last" lastFn
    , b2 "take" takeFn
    , b2 "drop" dropFn
    , b1 "reverse" reverseFn
    , b1 "sort" sortFn
    , b2 "sortBy" sortByFn
    , b1 "unique" uniqueFn
    , b2 "member" memberFn
    , b1 "concat" concatFn

    -- higher-order
    , b2 "map" mapFn
    , b2 "filter" filterFn
    , b3 "foldl" foldlFn

    -- strings
    , b1 "toUpper" (mapStr String.toUpper)
    , b1 "toLower" (mapStr String.toLower)
    , b1 "trim" (mapStr String.trim)
    , b1 "words" wordsFn
    , b2 "split" splitFn
    , b2 "join" joinFn
    , b2 "contains" containsFn
    , b2 "startsWith" startsWithFn
    , b3 "replace" replaceFn
    , b1 "toText" toTextFn
    , b1 "toNumber" toNumberFn

    -- records & tables
    , b2 "get" getFn
    , b1 "keys" keysFn
    , b1 "values" valuesFn
    , b2 "column" columnFn
    , b2 "select" selectFn
    , b2 "sortByField" sortByFieldFn
    , b2 "groupBy" groupByFn
    , b1 "count" lengthFn
    , b1 "identity" Ok
    ]



-- builders -------------------------------------------------------------------


b1 : String -> (Value -> Result String Value) -> ( String, Value )
b1 name f =
    ( name, VBuiltin (Builtin name 1 [] (\args -> arg1 args |> Result.andThen f)) )


b2 : String -> (Value -> Value -> Result String Value) -> ( String, Value )
b2 name f =
    ( name, VBuiltin (Builtin name 2 [] (\args -> arg2 args |> Result.andThen (\( a, b ) -> f a b))) )


b3 : String -> (Value -> Value -> Value -> Result String Value) -> ( String, Value )
b3 name f =
    ( name, VBuiltin (Builtin name 3 [] (\args -> arg3 args |> Result.andThen (\( a, b, c ) -> f a b c))) )


arg1 : List Value -> Result String Value
arg1 args =
    case args of
        [ a ] ->
            Ok a

        _ ->
            Err "internal arity error"


arg2 : List Value -> Result String ( Value, Value )
arg2 args =
    case args of
        [ a, b ] ->
            Ok ( a, b )

        _ ->
            Err "internal arity error"


arg3 : List Value -> Result String ( Value, Value, Value )
arg3 args =
    case args of
        [ a, b, c ] ->
            Ok ( a, b, c )

        _ ->
            Err "internal arity error"



-- implementations ------------------------------------------------------------


mapNum : (Float -> Float) -> Value -> Result String Value
mapNum f v =
    asNum v |> Result.map (\n -> VNum (f n))


mapStr : (String -> String) -> Value -> Result String Value
mapStr f v =
    asStr v |> Result.map (\s -> VStr (f s))


num2 : (Float -> Float -> Float) -> Value -> Value -> Result String Value
num2 f a b =
    Result.map2 (\x y -> VNum (f x y)) (asNum a) (asNum b)


notFn : Value -> Result String Value
notFn v =
    case v of
        VBool b ->
            Ok (VBool (not b))

        _ ->
            Err ("`not` needs a bool, got a " ++ Value.typeName v)


modFn : Value -> Value -> Result String Value
modFn x m =
    Result.map2 Tuple.pair (asInt x) (asInt m)
        |> Result.andThen
            (\( a, b ) ->
                if b == 0 then
                    Err "`mod` by zero"

                else
                    Ok (VNum (toFloat (modBy b a)))
            )


clampFn : Value -> Value -> Value -> Result String Value
clampFn lo hi x =
    Result.map3 (\a b c -> VNum (Basics.clamp a b c)) (asNum lo) (asNum hi) (asNum x)


rangeFn : Value -> Value -> Result String Value
rangeFn lo hi =
    Result.map2
        (\a b -> VList (List.map (\n -> VNum (toFloat n)) (List.range a b)))
        (asInt lo)
        (asInt hi)


lengthFn : Value -> Result String Value
lengthFn v =
    case v of
        VList xs ->
            Ok (VNum (toFloat (List.length xs)))

        VStr s ->
            Ok (VNum (toFloat (String.length s)))

        _ ->
            Err ("`length` needs a list or text, got a " ++ Value.typeName v)


numbersOf : Value -> Result String (List Float)
numbersOf v =
    asList v
        |> Result.andThen
            (\xs ->
                List.foldr (\x acc -> Result.map2 (::) (asNum x) acc) (Ok []) xs
            )


numFold : (Float -> Float -> Float) -> Float -> Value -> Result String Value
numFold f seed v =
    numbersOf v |> Result.map (\ns -> VNum (List.foldl f seed ns))


numReduce : (Float -> Float -> Float) -> String -> Value -> Result String Value
numReduce f name v =
    numbersOf v
        |> Result.andThen
            (\ns ->
                case ns of
                    first :: rest ->
                        Ok (VNum (List.foldl f first rest))

                    [] ->
                        Err ("`" ++ name ++ "` of an empty list")
            )


meanFn : Value -> Result String Value
meanFn v =
    numbersOf v
        |> Result.andThen
            (\ns ->
                if List.isEmpty ns then
                    Err "`mean` of an empty list"

                else
                    Ok (VNum (List.sum ns / toFloat (List.length ns)))
            )


medianFn : Value -> Result String Value
medianFn v =
    numbersOf v
        |> Result.andThen
            (\ns ->
                let
                    sorted =
                        List.sort ns

                    n =
                        List.length sorted
                in
                if n == 0 then
                    Err "`median` of an empty list"

                else if modBy 2 n == 1 then
                    Ok (VNum (nthOr 0 (n // 2) sorted))

                else
                    Ok (VNum ((nthOr 0 (n // 2 - 1) sorted + nthOr 0 (n // 2) sorted) / 2))
            )


stddevFn : Value -> Result String Value
stddevFn v =
    numbersOf v
        |> Result.andThen
            (\ns ->
                let
                    n =
                        List.length ns
                in
                if n == 0 then
                    Err "`stddev` of an empty list"

                else
                    let
                        m =
                            List.sum ns / toFloat n

                        var =
                            List.sum (List.map (\x -> (x - m) ^ 2) ns) / toFloat n
                    in
                    Ok (VNum (sqrt var))
            )


nthOr : Float -> Int -> List Float -> Float
nthOr default i xs =
    List.drop i xs |> List.head |> Maybe.withDefault default


headFn : Value -> Result String Value
headFn v =
    asList v
        |> Result.andThen
            (\xs ->
                case xs of
                    first :: _ ->
                        Ok first

                    [] ->
                        Err "`head` of an empty list"
            )


lastFn : Value -> Result String Value
lastFn v =
    asList v
        |> Result.andThen
            (\xs ->
                case List.reverse xs of
                    first :: _ ->
                        Ok first

                    [] ->
                        Err "`last` of an empty list"
            )


takeFn : Value -> Value -> Result String Value
takeFn n v =
    Result.map2 (\k xs -> VList (List.take k xs)) (asInt n) (asList v)


dropFn : Value -> Value -> Result String Value
dropFn n v =
    Result.map2 (\k xs -> VList (List.drop k xs)) (asInt n) (asList v)


reverseFn : Value -> Result String Value
reverseFn v =
    asList v |> Result.map (\xs -> VList (List.reverse xs))


concatFn : Value -> Result String Value
concatFn v =
    asList v
        |> Result.andThen
            (\xs ->
                List.foldr
                    (\x acc -> Result.map2 (\inner rest -> inner ++ rest) (asList x) acc)
                    (Ok [])
                    xs
            )
        |> Result.map VList


memberFn : Value -> Value -> Result String Value
memberFn needle v =
    asList v |> Result.map (\xs -> VBool (List.any (Value.equalValue needle) xs))


uniqueFn : Value -> Result String Value
uniqueFn v =
    asList v
        |> Result.map
            (\xs ->
                VList
                    (List.foldl
                        (\x acc ->
                            if List.any (Value.equalValue x) acc then
                                acc

                            else
                                acc ++ [ x ]
                        )
                        []
                        xs
                    )
            )



-- sorting (by a derived comparable key) --------------------------------------


sortKey : Value -> ( Int, Float, String )
sortKey v =
    case v of
        VNum n ->
            ( 0, n, "" )

        VBool b ->
            ( 0, boolRank b |> toFloat, "" )

        VStr s ->
            ( 1, 0, s )

        _ ->
            ( 2, 0, Value.toInline v )


sortFn : Value -> Result String Value
sortFn v =
    asList v |> Result.map (\xs -> VList (List.sortBy sortKey xs))


sortByFn : Value -> Value -> Result String Value
sortByFn f v =
    asList v
        |> Result.andThen
            (\xs ->
                keyed f xs
                    |> Result.map
                        (\pairs ->
                            VList (List.map Tuple.second (List.sortBy (\( k, _ ) -> sortKey k) pairs))
                        )
            )


keyed : Value -> List Value -> Result String (List ( Value, Value ))
keyed f xs =
    List.foldr
        (\x acc ->
            Result.map2 (\k rest -> ( k, x ) :: rest) (apply f x) acc
        )
        (Ok [])
        xs


sortByFieldFn : Value -> Value -> Result String Value
sortByFieldFn fieldV table =
    asStr fieldV
        |> Result.andThen
            (\field ->
                asList table
                    |> Result.andThen
                        (\rows ->
                            List.foldr
                                (\row acc ->
                                    Result.map2 (\k rest -> ( k, row ) :: rest)
                                        (asRecord row |> Result.andThen (fieldValue field))
                                        acc
                                )
                                (Ok [])
                                rows
                                |> Result.map
                                    (\pairs ->
                                        VList (List.map Tuple.second (List.sortBy (\( k, _ ) -> sortKey k) pairs))
                                    )
                        )
            )


fieldValue : String -> List ( String, Value ) -> Result String Value
fieldValue field fields =
    case lookup field fields of
        Just v ->
            Ok v

        Nothing ->
            Err ("row has no field `" ++ field ++ "`")



-- higher-order ---------------------------------------------------------------


mapFn : Value -> Value -> Result String Value
mapFn f v =
    asList v
        |> Result.andThen
            (\xs -> List.foldr (\x acc -> Result.map2 (::) (apply f x) acc) (Ok []) xs)
        |> Result.map VList


filterFn : Value -> Value -> Result String Value
filterFn f v =
    asList v
        |> Result.andThen
            (\xs ->
                List.foldr
                    (\x acc ->
                        Result.map2
                            (\keep rest ->
                                case keep of
                                    VBool True ->
                                        x :: rest

                                    _ ->
                                        rest
                            )
                            (apply f x)
                            acc
                    )
                    (Ok [])
                    xs
            )
        |> Result.map VList


foldlFn : Value -> Value -> Value -> Result String Value
foldlFn f seed v =
    asList v
        |> Result.andThen
            (\xs ->
                List.foldl
                    (\x acc -> acc |> Result.andThen (\state -> apply f x |> Result.andThen (\g -> apply g state)))
                    (Ok seed)
                    xs
            )



-- strings --------------------------------------------------------------------


wordsFn : Value -> Result String Value
wordsFn v =
    asStr v |> Result.map (\s -> VList (List.map VStr (String.words s)))


splitFn : Value -> Value -> Result String Value
splitFn sep v =
    Result.map2 (\s str -> VList (List.map VStr (String.split s str))) (asStr sep) (asStr v)


joinFn : Value -> Value -> Result String Value
joinFn sep v =
    asStr sep
        |> Result.andThen
            (\s ->
                asList v
                    |> Result.andThen
                        (\xs ->
                            List.foldr (\x acc -> Result.map2 (::) (asStr x) acc) (Ok []) xs
                                |> Result.map (\strs -> VStr (String.join s strs))
                        )
            )


containsFn : Value -> Value -> Result String Value
containsFn sub v =
    Result.map2 (\a b -> VBool (String.contains a b)) (asStr sub) (asStr v)


startsWithFn : Value -> Value -> Result String Value
startsWithFn pre v =
    Result.map2 (\a b -> VBool (String.startsWith a b)) (asStr pre) (asStr v)


replaceFn : Value -> Value -> Value -> Result String Value
replaceFn from to v =
    Result.map3 (\a b c -> VStr (String.replace a b c)) (asStr from) (asStr to) (asStr v)


toTextFn : Value -> Result String Value
toTextFn v =
    Ok (VStr (Value.toDisplayString v))


toNumberFn : Value -> Result String Value
toNumberFn v =
    case v of
        VNum _ ->
            Ok v

        VStr s ->
            case String.toFloat (String.trim s) of
                Just n ->
                    Ok (VNum n)

                Nothing ->
                    Err ("cannot read `" ++ s ++ "` as a number")

        _ ->
            Err ("cannot read a " ++ Value.typeName v ++ " as a number")



-- records & tables -----------------------------------------------------------


getFn : Value -> Value -> Result String Value
getFn fieldV recV =
    asStr fieldV
        |> Result.andThen
            (\field -> asRecord recV |> Result.andThen (fieldValue field))


keysFn : Value -> Result String Value
keysFn v =
    asRecord v |> Result.map (\fs -> VList (List.map (\( k, _ ) -> VStr k) fs))


valuesFn : Value -> Result String Value
valuesFn v =
    asRecord v |> Result.map (\fs -> VList (List.map Tuple.second fs))


columnFn : Value -> Value -> Result String Value
columnFn fieldV table =
    asStr fieldV
        |> Result.andThen
            (\field ->
                asList table
                    |> Result.andThen
                        (\rows ->
                            List.foldr
                                (\row acc ->
                                    Result.map2 (::)
                                        (asRecord row |> Result.andThen (fieldValue field))
                                        acc
                                )
                                (Ok [])
                                rows
                        )
            )
        |> Result.map VList


selectFn : Value -> Value -> Result String Value
selectFn namesV table =
    asList namesV
        |> Result.andThen
            (\nameVals ->
                List.foldr (\x acc -> Result.map2 (::) (asStr x) acc) (Ok []) nameVals
                    |> Result.andThen
                        (\names ->
                            asList table
                                |> Result.andThen
                                    (\rows ->
                                        List.foldr
                                            (\row acc ->
                                                Result.map2 (::) (projectRow names row) acc
                                            )
                                            (Ok [])
                                            rows
                                    )
                        )
            )
        |> Result.map VList


projectRow : List String -> Value -> Result String Value
projectRow names row =
    asRecord row
        |> Result.andThen
            (\fields ->
                List.foldr
                    (\name acc ->
                        Result.map2 (\v rest -> ( name, v ) :: rest)
                            (fieldValue name fields)
                            acc
                    )
                    (Ok [])
                    names
            )
        |> Result.map VRecord


groupByFn : Value -> Value -> Result String Value
groupByFn fieldV table =
    asStr fieldV
        |> Result.andThen
            (\field ->
                asList table
                    |> Result.andThen
                        (\rows ->
                            List.foldl
                                (\row acc ->
                                    acc
                                        |> Result.andThen
                                            (\groups ->
                                                asRecord row
                                                    |> Result.andThen (fieldValue field)
                                                    |> Result.map (\k -> addToGroup k row groups)
                                            )
                                )
                                (Ok [])
                                rows
                        )
            )
        |> Result.map
            (\groups ->
                VList
                    (List.map
                        (\( _, key, rows ) ->
                            VRecord
                                [ ( "key", key )
                                , ( "count", VNum (toFloat (List.length rows)) )
                                , ( "rows", VList rows )
                                ]
                        )
                        (List.reverse groups)
                    )
            )


{-| Accumulate a row under its key, preserving first-seen group order. Groups are kept as
`(keyString, keyValue, rowsReversed?)` — here rows are appended so order within a group is
preserved; the outer list is built reversed and flipped by the caller.
-}
addToGroup : Value -> Value -> List ( String, Value, List Value ) -> List ( String, Value, List Value )
addToGroup key row groups =
    let
        keyStr =
            Value.toInline key
    in
    if List.any (\( k, _, _ ) -> k == keyStr) groups then
        List.map
            (\( k, kv, rows ) ->
                if k == keyStr then
                    ( k, kv, rows ++ [ row ] )

                else
                    ( k, kv, rows )
            )
            groups

    else
        ( keyStr, key, [ row ] ) :: groups
