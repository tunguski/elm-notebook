module Notebook.Prelude exposing (source)

{-| A tiny standard prelude for the notebook, written in **real Elm** and loaded as the base
global scope every notebook starts with. The interpreter already provides the full `List`,
`String`, `Dict`, `Maybe`, `Result` and math libraries; this only adds the few data-exploration
conveniences they lack — an average, a generic group-by, and a de-duplicator — so a cell can
write `mean (List.map .salary people)` or `groupBy .dept people` directly.

Because it is ordinary Elm, every name here is also something the learner can read and imitate.

@docs source

-}


{-| The prelude source, parsed into the kernel's base `Globals`. -}
source : String
source =
    """
mean numbers =
    List.sum numbers / toFloat (List.length numbers)


total numbers =
    List.sum numbers


unique items =
    List.foldl
        (\\x seen -> if List.member x seen then seen else seen ++ [ x ])
        []
        items


groupBy keyFn items =
    List.foldl (\\x groups -> groupAdd (keyFn x) x groups) [] items


groupAdd key x groups =
    case groups of
        [] ->
            [ { key = key, count = 1, items = [ x ] } ]

        g :: rest ->
            if g.key == key then
                { key = g.key, count = g.count + 1, items = g.items ++ [ x ] } :: rest

            else
                g :: groupAdd key x rest
"""
