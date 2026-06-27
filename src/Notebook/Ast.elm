module Notebook.Ast exposing (Expr(..), CellForm(..))

{-| The abstract syntax of the little Elm-flavoured expression language a notebook
code cell is written in.

A code cell is a single expression — optionally a top-level binding `name = expr`
that publishes `name` into the kernel environment so later cells can use it (this is
how a Jupyter kernel keeps state between cells). Everything else is an ordinary
expression: literals, lists, records, lambdas, function application, the usual
operators, `if`/`then`/`else` and `let`/`in`.

@docs Expr, CellForm

-}


{-| An expression node. Multi-argument lambdas desugar to nested `ELambda`s and the
pipe operators desugar to `EApply`, so the evaluator stays tiny.
-}
type Expr
    = ENum Float
    | EStr String
    | EBool Bool
    | EList (List Expr)
    | ERecord (List ( String, Expr ))
    | EVar String
    | EField Expr String
    | ELambda String Expr
    | EApply Expr Expr
    | EBinop String Expr Expr
    | ENeg Expr
    | EIf Expr Expr Expr
    | ELet (List ( String, Expr )) Expr


{-| What a whole cell's source parses to: either a binding that names its result, or a
bare expression whose value is simply displayed.
-}
type CellForm
    = CBind String Expr
    | CBare Expr
