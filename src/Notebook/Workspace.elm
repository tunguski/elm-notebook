module Notebook.Workspace exposing (NbDoc, NbMsg, config, examples, examplesView, updateNb, isCopyToWorkspace)

{-| The notebook seen as a **workspace document**: this module adapts the notebook engine
([`Notebook.Doc`](Notebook-Doc) / [`Cell`](Notebook-Cell) / [`Kernel`](Notebook-Kernel) /
[`View`](Notebook-View)) to the generic [`Workspace`](Workspace) component, so a notebook gains
naming, opening, search, copy, sharing/permissions, comments, URL import, SQL and export — all from
the shared library — while keeping its own editor (highlighted cells, suggestions, variables, input
widgets, charts, per-step export).

The workspace `doc` is an [`NbDoc`](#NbDoc): the persisted notebook document plus the *transient*
editor UI (caret positions, per-cell chart choice, the active lesson). Only the inner
`Notebook.Doc` is serialised; the UI is rebuilt on open. All the per-cell editing that used to live
in `Main` is now [`NbMsg`](#NbMsg) handled here.

@docs NbDoc, NbMsg, config

-}

import Dict exposing (Dict)
import Html exposing (Html, button, div, section, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Notebook.Chart as Chart
import Notebook.Csv as Csv
import Notebook.Deps as Deps
import Notebook.Doc as Doc exposing (Doc)
import Notebook.Cell as Cell exposing (Cell, CellKind(..), Control(..), Output(..))
import Notebook.Export as Export
import Notebook.Hint as Hint
import Notebook.Kernel as Kernel
import Notebook.Serialize as Serialize
import Notebook.Suggest as Suggest exposing (Lesson, Suggestion)
import Notebook.View as NbView
import Set exposing (Set)
import Workspace
import Workspace.Table as Table
import Workspace.Types exposing (Table)


{-| A notebook document plus its transient editor state. `stale` holds the cells whose displayed
output no longer reflects an upstream edit (see [`Notebook.Deps`](Notebook-Deps)). -}
type alias NbDoc =
    { doc : Doc
    , carets : Dict Int Int
    , charts : Dict Int Chart.ChartKind
    , cols : Dict Int String
    , tables : Dict Int TableState
    , lesson : String
    , stale : Set Int
    }


{-| Per-cell interactive-table state: the sort column + direction, the row filter, and whether the
row cap is lifted. Updated here (the owning module) to avoid a cross-module record-update miscompile. -}
type alias TableState =
    { sortCol : Maybe String, desc : Bool, filter : String, expanded : Bool }


defaultTable : TableState
defaultTable =
    { sortCol = Nothing, desc = False, filter = "", expanded = False }


tableOf : Int -> NbDoc -> TableState
tableOf id nb =
    Dict.get id nb.tables |> Maybe.withDefault defaultTable


{-| The notebook editor's messages (everything the old single-notebook `Main` handled). -}
type NbMsg
    = Edit Int String Int
    | Run Int
    | RunAll
    | RunStale
    | Fix Int String
    | DeleteCell Int
    | MoveUp Int
    | MoveDown Int
    | Convert Int CellKind
    | Insert Suggestion
    | AddCode
    | AddMarkdown
    | AddInput
    | Clear
    | LoadLesson Lesson
    | SetChart Int (Maybe Chart.ChartKind)
    | SetCol Int String
    | SortBy Int String
    | FilterRows Int String
    | ExpandTable Int Bool
    | InsertName String
    | SetInputValue Int String
    | SetInputName Int String
    | SetInputControl Int String
    | CopyToWorkspaceRequested



-- CONFIG ---------------------------------------------------------------------


{-| The notebook's workspace configuration. -}
config : Workspace.Config NbDoc NbMsg
config =
    { codec = { encode = \nb -> Serialize.encodeDoc nb.doc, decoder = decoder }
    , empty = empty
    , kind = "notebook"
    , activate = \nb -> { nb | doc = Doc.runAll nb.doc }
    , viewDoc = viewNb False
    , updateDoc = updateNb
    , elementsOf = elementsOf
    , toTable = \nb -> Doc.lastValue nb.doc |> Maybe.andThen Export.valueToTable
    , onImport = Just importTable
    }


decoder : D.Decoder NbDoc
decoder =
    D.map (\d -> { doc = d, carets = Dict.empty, charts = Dict.empty, cols = Dict.empty, tables = Dict.empty, lesson = "", stale = Set.empty }) Serialize.decoder


empty : NbDoc
empty =
    { doc =
        Doc.empty
            |> Doc.append Markdown "# New notebook\n\nDescribe what you're exploring, then add a code cell."
            |> Doc.append Code ""
            |> Doc.runAll
    , carets = Dict.empty
    , charts = Dict.empty
    , cols = Dict.empty
    , tables = Dict.empty
    , lesson = ""
    , stale = Set.empty
    }


{-| The starter notebook used by the standalone playground (the site's "examples" landing) — the
guided starter run and ready to explore, without any workspace chrome. -}
examples : NbDoc
examples =
    { doc = Doc.fromSpec Suggest.starter |> Doc.runAll
    , carets = Dict.empty
    , charts = Dict.empty
    , cols = Dict.empty
    , tables = Dict.empty
    , lesson = "starter"
    , stale = Set.empty
    }


{-| Render a notebook as a standalone playground (no comments, no workspace chrome), with a
"Copy to workspace" action in its toolbars. -}
examplesView : NbDoc -> Html NbMsg
examplesView nb =
    viewNb True { comments = Dict.empty, commentsVisible = False, commentCount = always 0 } nb


elementsOf : NbDoc -> List ( String, String )
elementsOf nb =
    List.map (\c -> ( String.fromInt c.id, cellLabel c )) nb.doc.cells


cellLabel : Cell -> String
cellLabel c =
    let
        kind =
            case c.kind of
                Markdown ->
                    "Text"

                Input ->
                    "Input"

                Code ->
                    "Code"
    in
    kind ++ " cell #" ++ String.fromInt c.id


importTable : Table -> NbDoc -> NbDoc
importTable table nb =
    let
        source =
            case Csv.toElm "imported" (Table.toCsv table) of
                Ok src ->
                    src

                Err message ->
                    "-- could not import data: " ++ message
    in
    { nb | doc = nb.doc |> Doc.append Code source |> Doc.runAll }



-- UPDATE ---------------------------------------------------------------------


updateNb : NbMsg -> NbDoc -> NbDoc
updateNb msg nb =
    case msg of
        Edit id source caret ->
            let
                doc2 =
                    Doc.setSource id source nb.doc
            in
            -- the edited cell and everything downstream of it now need re-running
            { nb
                | doc = doc2
                , carets = Dict.insert id caret nb.carets
                , stale = Set.union nb.stale (Deps.affected id doc2)
            }

        Run id ->
            -- reactively refresh this cell and exactly the cells that depend on it
            let
                hit =
                    Deps.affected id nb.doc
            in
            { nb | doc = Doc.runAffected hit nb.doc, stale = Set.diff nb.stale hit }

        RunAll ->
            { nb | doc = Doc.runAll nb.doc, stale = Set.empty }

        RunStale ->
            { nb | doc = Doc.runAffected nb.stale nb.doc, stale = Set.empty }

        Fix id src ->
            -- a one-click "did you mean…?" fix: replace the cell's source and re-run what it affects
            let
                doc2 =
                    Doc.setSource id src nb.doc

                hit =
                    Deps.affected id doc2
            in
            { nb | doc = Doc.runAffected hit doc2, stale = Set.diff nb.stale hit }

        DeleteCell id ->
            { nb | doc = Doc.remove id nb.doc }

        MoveUp id ->
            { nb | doc = Doc.moveUp id nb.doc }

        MoveDown id ->
            { nb | doc = Doc.moveDown id nb.doc }

        Convert id kind ->
            { nb | doc = Doc.setKind id kind nb.doc }

        Insert suggestion ->
            { nb | doc = nb.doc |> Doc.append suggestion.kind suggestion.source |> Doc.runAll }

        AddCode ->
            { nb | doc = Doc.append Code "" nb.doc }

        AddMarkdown ->
            { nb | doc = Doc.append Markdown "## Notes\n\n…" nb.doc }

        AddInput ->
            { nb | doc = Doc.appendInput defaultInput nb.doc |> Doc.runAll }

        Clear ->
            let
                doc2 =
                    Doc.clearOutputs nb.doc
            in
            { nb | doc = doc2, stale = Set.fromList (Doc.executableIds doc2) }

        LoadLesson lesson ->
            { nb | doc = Doc.fromSpec lesson.cells |> Doc.runAll, lesson = lesson.id, carets = Dict.empty, charts = Dict.empty, cols = Dict.empty, tables = Dict.empty, stale = Set.empty }

        SetChart id maybeKind ->
            { nb
                | charts =
                    case maybeKind of
                        Just kind ->
                            Dict.insert id kind nb.charts

                        Nothing ->
                            Dict.remove id nb.charts
            }

        SetCol id colName ->
            { nb | cols = Dict.insert id colName nb.cols }

        SortBy id colName ->
            let
                st =
                    tableOf id nb

                next =
                    if st.sortCol == Just colName then
                        { st | desc = not st.desc }

                    else
                        { st | sortCol = Just colName, desc = False }
            in
            { nb | tables = Dict.insert id next nb.tables }

        FilterRows id needle ->
            let
                st =
                    tableOf id nb
            in
            { nb | tables = Dict.insert id { st | filter = needle } nb.tables }

        ExpandTable id flag ->
            let
                st =
                    tableOf id nb
            in
            { nb | tables = Dict.insert id { st | expanded = flag } nb.tables }

        InsertName name ->
            { nb | doc = nb.doc |> Doc.append Code name |> Doc.runAll }

        SetInputValue id value ->
            { nb | doc = Doc.setInputValue id value nb.doc |> Doc.runAll }

        SetInputName id name ->
            { nb | doc = Doc.setInputName id name nb.doc |> Doc.runAll }

        SetInputControl id controlName ->
            { nb | doc = Doc.setInputControl id (parseControl controlName) nb.doc |> Doc.runAll }

        CopyToWorkspaceRequested ->
            -- handled by the host (see Main), which copies this notebook into the workspace
            nb


{-| Did this message ask to copy the standalone playground into the workspace? The host intercepts
it (the adapter can't reach the workspace state). -}
isCopyToWorkspace : NbMsg -> Bool
isCopyToWorkspace msg =
    msg == CopyToWorkspaceRequested


defaultInput : Cell.InputSpec
defaultInput =
    { name = "x", control = Slider 0 100 1, value = "50" }


parseControl : String -> Control
parseControl name =
    case name of
        "number" ->
            NumberBox

        "text" ->
            TextBox

        "checkbox" ->
            Checkbox

        _ ->
            Slider 0 100 1



-- VIEW -----------------------------------------------------------------------


viewNb : Bool -> Workspace.EditorEnv -> NbDoc -> Html NbMsg
viewNb showCopy env nb =
    div [ HA.class "nb-workspace" ]
        [ lessonBar nb.lesson
        , toolbar showCopy (Set.size nb.stale) nb.doc
        , section [ HA.class "nb-main" ]
            [ div [ HA.class "nb-notebook" ]
                [ NbView.notebook (viewConfig env nb) nb.doc
                , toolbar showCopy (Set.size nb.stale) nb.doc
                ]
            , div [ HA.class "nb-sidebar" ]
                [ NbView.errorsPanel Run (errorList nb.doc)
                , NbView.suggestionsPanel Insert (Suggest.suggestNext (Doc.lastValue nb.doc))
                , NbView.variablesPanel InsertName (Doc.variables nb.doc)
                ]
            ]
        ]


viewConfig : Workspace.EditorEnv -> NbDoc -> NbView.Config NbMsg
viewConfig env nb =
    { onEdit = Edit
    , onRun = Run
    , onDelete = DeleteCell
    , onMoveUp = MoveUp
    , onMoveDown = MoveDown
    , onConvert = Convert
    , onInsert = Insert
    , caretOf = \id -> Dict.get id nb.carets |> Maybe.withDefault 0
    , chartOf = \id -> Dict.get id nb.charts
    , onChart = SetChart
    , colOf = \id -> Dict.get id nb.cols
    , onCol = SetCol
    , onInputValue = SetInputValue
    , onInputName = SetInputName
    , onInputControl = SetInputControl
    , commentsVisible = env.commentsVisible
    , commentCountOf = \id -> env.commentCount (String.fromInt id)
    , exportCell = \cell -> exportCell cell
    , isStale = \id -> Set.member id nb.stale
    , errorFix = errorFix nb
    , onFix = Fix
    , tableSort = \id -> (tableOf id nb).sortCol |> Maybe.map (\c -> ( c, (tableOf id nb).desc ))
    , onSort = SortBy
    , tableFilter = \id -> (tableOf id nb).filter
    , onFilter = FilterRows
    , tableExpanded = \id -> (tableOf id nb).expanded
    , onExpand = ExpandTable
    }


{-| For a cell that failed on an unbound name, a "Did you mean …?" fix: the nearest in-scope name
and the cell's source with the first occurrence of the typo replaced. -}
errorFix : NbDoc -> Cell -> Maybe { label : String, fixed : String }
errorFix nb cell =
    case cell.output of
        OutError message ->
            Hint.unboundName message
                |> Maybe.andThen
                    (\wrong ->
                        Hint.closest wrong (Kernel.names nb.doc.kernel)
                            |> Maybe.map
                                (\good ->
                                    { label = "Did you mean " ++ good ++ "?"
                                    , fixed = replaceFirst wrong good cell.source
                                    }
                                )
                    )

        _ ->
            Nothing


{-| The cells currently in error, as `(id, count, message)` for the errors panel. -}
errorList : Doc -> List ( Int, Int, String )
errorList doc =
    doc.cells
        |> List.filterMap
            (\c ->
                case c.output of
                    OutError message ->
                        Just ( c.id, Maybe.withDefault 0 c.count, message )

                    _ ->
                        Nothing
            )


{-| Replace the first whole-word occurrence of `wrong` with `good` in `source`. -}
replaceFirst : String -> String -> String -> String
replaceFirst wrong good source =
    case String.indexes wrong source of
        i :: _ ->
            String.left i source ++ good ++ String.dropLeft (i + String.length wrong) source

        [] ->
            source


exportCell : Cell -> Html NbMsg
exportCell cell =
    case cell.output of
        Cell.OutValue value ->
            Export.cellLinks "step" value

        _ ->
            text ""


toolbar : Bool -> Int -> Doc -> Html NbMsg
toolbar showCopy staleCount doc =
    section [ HA.class "nb-actions" ]
        [ button [ HA.class "nb-action nb-action-primary", HE.onClick RunAll ] [ text "▶▶ Run all" ]
        , if staleCount > 0 then
            button [ HA.class "nb-action nb-action-stale", HE.onClick RunStale, HA.title "Re-run the cells affected by your edits" ]
                [ Html.i [ HA.class "bi bi-arrow-repeat" ] [], text (" Run stale (" ++ String.fromInt staleCount ++ ")") ]

          else
            text ""
        , button [ HA.class "nb-action", HE.onClick AddCode ] [ text "+ Code cell" ]
        , button [ HA.class "nb-action", HE.onClick AddMarkdown ] [ text "+ Text cell" ]
        , button [ HA.class "nb-action", HE.onClick AddInput ] [ text "+ Input" ]
        , button [ HA.class "nb-action", HE.onClick Clear ] [ text "Clear outputs" ]
        , span [ HA.class "nb-action-export" ] [ Export.notebookLinks doc ]
        , if showCopy then
            button [ HA.class "nb-action nb-action-copy", HE.onClick CopyToWorkspaceRequested ]
                [ Html.i [ HA.class "bi bi-folder-plus" ] [], text " Copy to workspace" ]

          else
            text ""
        ]


lessonBar : String -> Html NbMsg
lessonBar active =
    section [ HA.class "nb-lessons" ]
        [ span [ HA.class "nb-lessons-label" ] [ text "Guided lessons:" ]
        , div [ HA.class "nb-lesson-buttons" ]
            (List.map (lessonButton active) Suggest.lessons)
        ]


lessonButton : String -> Lesson -> Html NbMsg
lessonButton active lesson =
    button
        [ HA.class
            ("nb-lesson-btn"
                ++ (if lesson.id == active then
                        " nb-lesson-active"

                    else
                        ""
                   )
            )
        , HA.title lesson.blurb
        , HE.onClick (LoadLesson lesson)
        ]
        [ span [ HA.class "nb-lesson-name" ] [ text lesson.title ]
        , span [ HA.class "nb-lesson-blurb" ] [ text lesson.blurb ]
        ]
