module Notebook.Kernel exposing (Kernel, empty, seed, run, names, bindings)

{-| The notebook **kernel**: the stateful engine that executes code cells, the role the Python
process plays in Jupyter — but here the language is real Elm, run by the vendored
[`elm-in-elm` interpreter](Eval) (`Lexer` → `Parser` → `Eval.evalExpr`).

A kernel is a set of top-level definitions ([`Lang.Globals`](Lang#Globals) — one
mutually-recursive scope, seeded with [`Notebook.Prelude`](Notebook-Prelude)) plus an execution
counter. Running a cell:

  - a bare expression is evaluated against the globals and its value shown;
  - a `name = expr` (or `name args = body`) declaration is merged into the globals so later cells
    can use it;
  - either way `_` is bound to the value just produced, for REPL-style chaining.

Detection is expression-first: if the source parses as an expression it is one; otherwise it is
parsed as a declaration. This is unambiguous (`x = 5` is not a valid expression, `let … in …`
is) and means `case`, custom record literals, tuples and the whole real-Elm grammar Just Work.

@docs Kernel, empty, run, names

-}

import Dict
import Eval
import Lang exposing (Decl, Expr(..), Globals, Value)
import Lexer exposing (Token(..))
import Notebook.Cell exposing (Output(..))
import Notebook.Prelude as Prelude
import Parser


{-| Kernel state: the live global definitions and how many cells have run. -}
type alias Kernel =
    { globals : Globals
    , count : Int
    }


{-| A fresh kernel preloaded with the prelude (`mean`, `groupBy`, `unique`, …) on top of the
interpreter's built-in `List`/`String`/`Dict`/math libraries.
-}
empty : Kernel
empty =
    { globals =
        Parser.parseProject [ ( "Prelude", Prelude.source ) ]
            |> Result.withDefault Dict.empty
    , count = 0
    }


{-| A fresh kernel with extra top-level definitions merged in — each `defs` entry is a chunk of Elm
source (`name = expr`). Used to preload the tables a notebook pulls in from referenced documents so
they are in scope before any cell runs. The execution counter is reset to 0 so the first real cell
is still `[1]`. -}
seed : List String -> Kernel
seed defs =
    let
        seeded =
            List.foldl (\src k -> Tuple.second (run src k)) empty defs
    in
    { seeded | count = 0 }


{-| The names currently in scope (prelude + everything defined so far). -}
names : Kernel -> List String
names kernel =
    Dict.keys kernel.globals


{-| The names defined by the prelude — excluded from the variables inspector, which should show
only what the user has bound.
-}
preludeNames : List String
preludeNames =
    Parser.parseProject [ ( "Prelude", Prelude.source ) ]
        |> Result.map Dict.keys
        |> Result.withDefault []


{-| The user-defined bindings (everything except the prelude), each paired with its current value.
A binding whose value fails to evaluate (e.g. a function referencing an as-yet-undefined name) is
dropped. Used by the variables inspector.
-}
bindings : Kernel -> List ( String, Value )
bindings kernel =
    Dict.keys kernel.globals
        |> List.filter (\nm -> not (List.member nm preludeNames))
        |> List.filterMap
            (\nm ->
                Eval.evalGlobal kernel.globals nm
                    |> Result.toMaybe
                    |> Maybe.map (\v -> ( nm, v ))
            )


{-| Run a code cell's source, returning its output and the next kernel state. The execution
count advances on every run, even on error, like a real notebook. Empty cells are a no-op.
-}
run : String -> Kernel -> ( Output, Kernel )
run source kernel =
    if String.trim source == "" then
        ( OutNone, kernel )

    else
        let
            next =
                kernel.count + 1
        in
        case Lexer.tokenize source |> Result.andThen Parser.parse of
            Ok expr ->
                runExpr expr next kernel

            Err exprErr ->
                runDecls source exprErr next kernel


runExpr : Expr -> Int -> Kernel -> ( Output, Kernel )
runExpr expr next kernel =
    case Eval.evalExpr kernel.globals Dict.empty expr of
        Ok value ->
            ( OutValue value
            , { globals = bindBody expr kernel.globals, count = next }
            )

        Err message ->
            ( OutError message, { kernel | count = next } )


runDecls : String -> String -> Int -> Kernel -> ( Output, Kernel )
runDecls source exprErr next kernel =
    case Parser.parseModule source of
        Ok [] ->
            if isIgnorable source then
                -- Only comments / type or import headers — nothing to evaluate.
                ( OutNone, { kernel | count = next } )

            else
                -- Neither an expression nor a declaration — report the expression error.
                ( OutError exprErr, { kernel | count = next } )

        Ok decls ->
            let
                merged =
                    List.foldl (\( name, decl ) g -> Dict.insert name decl g) kernel.globals decls

                lastName =
                    decls |> List.reverse |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault "_"
            in
            case Eval.evalExpr merged Dict.empty (Var lastName) of
                Ok value ->
                    ( OutValue value
                    , { globals = bindBody (Var lastName) merged, count = next }
                    )

                Err message ->
                    ( OutError message, { globals = merged, count = next } )

        Err declErr ->
            ( OutError (chooseError source exprErr declErr), { kernel | count = next } )


{-| Bind `_` to the result of evaluating `body`, so the next cell can refer to it. -}
bindBody : Expr -> Globals -> Globals
bindBody body globals =
    Dict.insert "_" (Decl "_" [] body) globals


{-| When neither parse succeeded, show the message for the form the source most looks like. -}
chooseError : String -> String -> String -> String
chooseError source exprErr declErr =
    if looksLikeBinding source then
        declErr

    else
        exprErr


{-| A cell that legitimately produces no declarations: a comment, or a `module`/`import`/`type`/
`port` header the parser skips.
-}
isIgnorable : String -> Bool
isIgnorable source =
    let
        trimmed =
            String.trimLeft source

        firstWord =
            trimmed |> String.words |> List.head |> Maybe.withDefault ""
    in
    String.startsWith "--" trimmed
        || (trimmed == "")
        || List.member firstWord [ "module", "import", "type", "port" ]


looksLikeBinding : String -> Bool
looksLikeBinding source =
    case Lexer.tokenize source of
        Ok ((TId _) :: rest) ->
            scanForEquals rest

        _ ->
            False


scanForEquals : List Token -> Bool
scanForEquals tokens =
    case tokens of
        TEquals :: _ ->
            True

        (TId _) :: rest ->
            scanForEquals rest

        _ ->
            False
