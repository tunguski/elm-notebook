module Notebook.Deps exposing (defines, refs, affected, dependents)

{-| **Dependency analysis** between code cells, for reactive execution.

A code cell *defines* the names it binds (its declaration names, plus `_` — every executable cell
rebinds the REPL result), and *references* the lowercase identifiers it uses. By walking the cells
in document order and remembering which cell most recently produced each name, we learn for every
cell which earlier cell each of its bindings actually comes from — the real data-flow graph, not
just "everything above me".

That graph powers two things in [`Notebook.Workspace`](Notebook-Workspace): marking the cells
*downstream* of an edit as **stale**, and re-running exactly those cells (and only those) when one
is run — so changing a parameter cell refreshes the analyses that depend on it without touching
unrelated work.

@docs defines, refs, affected, dependents

-}

import Dict exposing (Dict)
import Lexer exposing (Token(..))
import Notebook.Cell as Cell exposing (Cell)
import Notebook.Doc exposing (Doc)
import Parser
import Set exposing (Set)


{-| Keywords the lexer emits as identifiers — never cross-cell bindings. -}
keywords : Set String
keywords =
    Set.fromList
        [ "let", "in", "if", "then", "else", "case", "of", "type", "module", "import", "port", "as", "exposing", "where" ]


{-| The names a cell binds when it runs: its declaration names plus `_` (the REPL result every
executable cell rebinds). Markdown cells bind nothing. -}
defines : Cell -> List String
defines cell =
    if Cell.isExecutable cell then
        "_" :: declNames cell.source

    else
        []


declNames : String -> List String
declNames source =
    case Parser.parseModule source of
        Ok decls ->
            List.map Tuple.first decls

        Err _ ->
            []


{-| The lowercase identifiers a cell references (a superset is fine — a spurious reference only
adds a dependency, it never drops one). Field accesses (`r.field`), constructors and qualified
module names are excluded. -}
refs : Cell -> List String
refs cell =
    if Cell.isExecutable cell then
        case Lexer.tokenize cell.source of
            Ok toks ->
                scanIds False toks []

            Err _ ->
                []

    else
        []


scanIds : Bool -> List Token -> List String -> List String
scanIds afterDot toks acc =
    case toks of
        [] ->
            List.reverse acc

        TDot :: rest ->
            scanIds True rest acc

        (TId name) :: rest ->
            if afterDot || Set.member name keywords then
                scanIds False rest acc

            else
                scanIds False rest (name :: acc)

        _ :: rest ->
            scanIds False rest acc


{-| For each cell, the ids of the cells that DIRECTLY depend on it (reference a binding it most
recently produced). -}
dependents : Doc -> Dict Int (Set Int)
dependents doc =
    let
        step cell ( producer, deps ) =
            let
                upstream =
                    refs cell
                        |> List.filterMap (\nm -> Dict.get nm producer)
                        |> List.filter (\pid -> pid /= cell.id)

                deps2 =
                    List.foldl
                        (\pid d -> Dict.update pid (addId cell.id) d)
                        deps
                        upstream

                producer2 =
                    List.foldl (\nm p -> Dict.insert nm cell.id p) producer (defines cell)
            in
            ( producer2, deps2 )
    in
    List.foldl step ( Dict.empty, Dict.empty ) doc.cells |> Tuple.second


addId : Int -> Maybe (Set Int) -> Maybe (Set Int)
addId id existing =
    Just (Set.insert id (Maybe.withDefault Set.empty existing))


{-| The cells whose output an edit to `cellId` affects: `cellId` itself plus every cell that
transitively (in document order) depends on a binding it produces. -}
affected : Int -> Doc -> Set Int
affected cellId doc =
    closure (dependents doc) (Set.singleton cellId) [ cellId ]


closure : Dict Int (Set Int) -> Set Int -> List Int -> Set Int
closure deps seen frontier =
    case frontier of
        [] ->
            seen

        id :: rest ->
            let
                fresh =
                    Dict.get id deps
                        |> Maybe.withDefault Set.empty
                        |> Set.filter (\x -> not (Set.member x seen))
            in
            closure deps (Set.union seen fresh) (rest ++ Set.toList fresh)
