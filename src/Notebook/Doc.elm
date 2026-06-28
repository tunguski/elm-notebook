module Notebook.Doc exposing
    ( Doc, empty, fromSpec
    , append, appendInput, insertAfter, remove, moveUp, moveDown
    , setSource, setKind, setInputValue, setInputName, setInputControl
    , runAll, runThrough, runAffected, clearOutputs
    , find, lastValue, codeCount, variables, executableIds
    )

{-| The notebook **document**: an ordered list of [`Cell`](Notebook-Cell#Cell)s plus the
[`Kernel`](Notebook-Kernel#Kernel) that runs them — the Jupyter `.ipynb` to the kernel's process.

Editing (add / move / remove / retype / edit) and running are pure transformations.
[`runAll`](#runAll) re-executes every code cell from a fresh kernel, so the displayed outputs
are always a faithful, reproducible function of the source — no hidden out-of-order state.

@docs Doc, empty, fromSpec
@docs append, insertAfter, remove, moveUp, moveDown
@docs setSource, setKind
@docs runAll, runThrough, clearOutputs
@docs find, lastValue, codeCount

-}

import Lang exposing (Value)
import Notebook.Cell as Cell exposing (Cell, CellKind(..), InputSpec, Output(..))
import Notebook.Kernel as Kernel exposing (Kernel)
import Set exposing (Set)


{-| A notebook: its cells, the id to hand the next new cell, and the current kernel. -}
type alias Doc =
    { cells : List Cell
    , nextId : Int
    , kernel : Kernel
    }


{-| An empty notebook with a fresh kernel. -}
empty : Doc
empty =
    { cells = [], nextId = 1, kernel = Kernel.empty }


{-| Build a notebook from a list of `(kind, source)` pairs — used by the lesson templates. -}
fromSpec : List ( CellKind, String ) -> Doc
fromSpec spec =
    List.foldl (\( kind, source ) doc -> append kind source doc) empty spec


newCell : Int -> CellKind -> String -> Cell
newCell id kind source =
    case kind of
        Markdown ->
            Cell.markdown id source

        Code ->
            Cell.code id source

        Input ->
            -- input cells are created via `appendInput`; this is a defensive fallback.
            Cell.code id source


{-| Append a cell to the end of the notebook. -}
append : CellKind -> String -> Doc -> Doc
append kind source doc =
    { doc
        | cells = doc.cells ++ [ newCell doc.nextId kind source ]
        , nextId = doc.nextId + 1
    }


{-| Append an input-widget cell. -}
appendInput : InputSpec -> Doc -> Doc
appendInput spec doc =
    { doc
        | cells = doc.cells ++ [ Cell.inputCell doc.nextId spec ]
        , nextId = doc.nextId + 1
    }


{-| Insert a new cell directly after the cell with the given id. -}
insertAfter : Int -> CellKind -> String -> Doc -> Doc
insertAfter targetId kind source doc =
    let
        cell =
            newCell doc.nextId kind source

        place existing =
            if existing.id == targetId then
                [ existing, cell ]

            else
                [ existing ]
    in
    { doc
        | cells = List.concatMap place doc.cells
        , nextId = doc.nextId + 1
    }


{-| Remove a cell by id. -}
remove : Int -> Doc -> Doc
remove targetId doc =
    { doc | cells = List.filter (\c -> c.id /= targetId) doc.cells }


{-| Move a cell one position earlier. -}
moveUp : Int -> Doc -> Doc
moveUp targetId doc =
    { doc | cells = swapBefore targetId doc.cells }


{-| Move a cell one position later. -}
moveDown : Int -> Doc -> Doc
moveDown targetId doc =
    { doc | cells = List.reverse (swapBefore targetId (List.reverse doc.cells)) }


swapBefore : Int -> List Cell -> List Cell
swapBefore targetId cells =
    case cells of
        a :: b :: rest ->
            if b.id == targetId then
                b :: a :: rest

            else
                a :: swapBefore targetId (b :: rest)

        _ ->
            cells


{-| Replace a cell's source, clearing the (now stale) output. -}
setSource : Int -> String -> Doc -> Doc
setSource targetId source doc =
    mapCell targetId (\c -> { c | source = source, output = OutNone, count = Nothing }) doc


{-| Switch a cell between Markdown and Code, clearing any stale output. -}
setKind : Int -> CellKind -> Doc -> Doc
setKind targetId kind doc =
    mapCell targetId (\c -> { c | kind = kind, output = OutNone, count = Nothing }) doc


{-| Set an input cell's value (re-syncs its `name = …` source). -}
setInputValue : Int -> String -> Doc -> Doc
setInputValue targetId value doc =
    mapCell targetId (Cell.setInputValue value) doc


{-| Rename an input cell's bound name. -}
setInputName : Int -> String -> Doc -> Doc
setInputName targetId name doc =
    mapCell targetId (Cell.setInputName name) doc


{-| Change an input cell's control type. -}
setInputControl : Int -> Cell.Control -> Doc -> Doc
setInputControl targetId control doc =
    mapCell targetId (Cell.setInputControl control) doc


mapCell : Int -> (Cell -> Cell) -> Doc -> Doc
mapCell targetId f doc =
    { doc
        | cells =
            List.map
                (\c ->
                    if c.id == targetId then
                        f c

                    else
                        c
                )
                doc.cells
    }


{-| Re-run the whole notebook from a fresh kernel. -}
runAll : Doc -> Doc
runAll doc =
    runFrom Nothing doc


{-| Re-run from a fresh kernel up to and including the given cell; later cells keep their
previous outputs. (Used by per-cell "Run", so a cell's result is always consistent with every
cell above it.)
-}
runThrough : Int -> Doc -> Doc
runThrough targetId doc =
    runFrom (Just targetId) doc


runFrom : Maybe Int -> Doc -> Doc
runFrom stopAt doc =
    let
        step cell ( kernel, done, acc ) =
            if done then
                ( kernel, done, cell :: acc )

            else if Cell.isExecutable cell then
                let
                    ( output, kernel2 ) =
                        Kernel.run cell.source kernel

                    count =
                        if output == OutNone && cell.source == "" then
                            Nothing

                        else
                            Just kernel2.count
                in
                ( kernel2
                , stopAt == Just cell.id
                , { cell | output = output, count = count } :: acc
                )

            else
                ( kernel
                , stopAt == Just cell.id
                , { cell | output = OutNone, count = Nothing } :: acc
                )

        ( finalKernel, _, reversed ) =
            List.foldl step ( Kernel.empty, False, [] ) doc.cells
    in
    { doc | cells = List.reverse reversed, kernel = finalKernel }


{-| Reactively re-run: execute every cell from a fresh kernel (so all later bindings stay correct),
but only **refresh the displayed output** of the cells in `affectedIds`; the rest keep their last
output. Used by [`Notebook.Deps`](Notebook-Deps)-driven "run what this change affects". -}
runAffected : Set Int -> Doc -> Doc
runAffected affectedIds doc =
    let
        step cell ( kernel, acc ) =
            if Cell.isExecutable cell then
                let
                    ( output, kernel2 ) =
                        Kernel.run cell.source kernel
                in
                if Set.member cell.id affectedIds then
                    ( kernel2, { cell | output = output, count = Just kernel2.count } :: acc )

                else
                    ( kernel2, cell :: acc )

            else
                ( kernel, cell :: acc )

        ( finalKernel, reversed ) =
            List.foldl step ( Kernel.empty, [] ) doc.cells
    in
    { doc | cells = List.reverse reversed, kernel = finalKernel }


{-| The ids of every executable (code / input) cell, in order. -}
executableIds : Doc -> List Int
executableIds doc =
    doc.cells |> List.filter Cell.isExecutable |> List.map .id


{-| Clear every output and reset the kernel. -}
clearOutputs : Doc -> Doc
clearOutputs doc =
    { doc
        | kernel = Kernel.empty
        , cells = List.map (\c -> { c | output = OutNone, count = Nothing }) doc.cells
    }


{-| Find a cell by id. -}
find : Int -> Doc -> Maybe Cell
find targetId doc =
    List.filter (\c -> c.id == targetId) doc.cells |> List.head


{-| The value produced by the most recent code cell that yielded one — what the suggestion
engine inspects to propose a next step.
-}
lastValue : Doc -> Maybe Value
lastValue doc =
    List.foldl
        (\c acc ->
            case c.output of
                OutValue v ->
                    Just v

                _ ->
                    acc
        )
        Nothing
        doc.cells


{-| How many code cells the notebook has. -}
codeCount : Doc -> Int
codeCount doc =
    List.length (List.filter Cell.isCode doc.cells)


{-| The user-defined kernel bindings (name, value) for the variables inspector. -}
variables : Doc -> List ( String, Value )
variables doc =
    Kernel.bindings doc.kernel
