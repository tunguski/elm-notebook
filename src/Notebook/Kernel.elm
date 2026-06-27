module Notebook.Kernel exposing (Kernel, empty, run, names)

{-| The notebook **kernel**: the stateful engine that executes code cells, exactly the role
the Python kernel plays in Jupyter.

A kernel is an environment (the names defined so far) plus an execution counter. Running a
code cell evaluates its source against the current environment and returns the
[`Output`](Notebook-Cell#Output) together with the new kernel state. A top-level
`name = expr` binding publishes `name` so later cells can use it; every successful run also
binds `_` to the value just produced, so cells can be chained REPL-style.

@docs Kernel, empty, run, names

-}

import Dict
import Notebook.Ast exposing (CellForm(..))
import Notebook.Cell exposing (Output(..))
import Notebook.Eval as Eval
import Notebook.Parser as Parser
import Notebook.Value exposing (Env, Value)


{-| Kernel state: the live environment and how many cells have been executed. -}
type alias Kernel =
    { env : Env
    , count : Int
    }


{-| A fresh kernel preloaded with the standard library. -}
empty : Kernel
empty =
    { env = Eval.defaultEnv, count = 0 }


{-| The names currently in scope (standard library + everything defined so far). -}
names : Kernel -> List String
names kernel =
    Dict.keys kernel.env


{-| Run a code cell's source against the kernel, returning its output and the next kernel
state. The execution count always advances — even on error — like a real notebook.
-}
run : String -> Kernel -> ( Output, Kernel )
run source kernel =
    let
        n =
            kernel.count + 1
    in
    case Parser.parseCell source of
        Err message ->
            ( OutError message, { kernel | count = n } )

        Ok (CBind name expr) ->
            case Eval.eval kernel.env expr of
                Ok value ->
                    ( OutValue value
                    , { env = bind name value kernel.env, count = n }
                    )

                Err message ->
                    ( OutError message, { kernel | count = n } )

        Ok (CBare expr) ->
            case Eval.eval kernel.env expr of
                Ok value ->
                    ( OutValue value
                    , { env = Dict.insert "_" value kernel.env, count = n }
                    )

                Err message ->
                    ( OutError message, { kernel | count = n } )


bind : String -> Value -> Env -> Env
bind name value env =
    env
        |> Dict.insert name value
        |> Dict.insert "_" value
