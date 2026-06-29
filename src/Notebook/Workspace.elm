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
import Lang
import Notebook.Chart as Chart
import Notebook.Csv as Csv
import Notebook.Deps as Deps
import Notebook.Doc as Doc exposing (Doc)
import Notebook.Cell as Cell exposing (Cell, CellKind(..), Control(..), Output(..))
import Notebook.Export as Export
import Notebook.Hint as Hint
import Notebook.Import as Import
import Notebook.Kernel as Kernel
import Notebook.Outline as Outline
import Notebook.Pivot as Pivot
import Notebook.Reference as Reference
import Notebook.Serialize as Serialize
import Notebook.Share as Share
import Notebook.Slides as Slides
import Notebook.Suggest as Suggest exposing (Lesson, Suggestion)
import Notebook.Templates as Templates
import Notebook.Value as Value
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
    , profiles : Set Int
    , pivots : Dict Int Pivot.Spec
    , corrs : Set Int
    , heats : Set Int
    , footers : Set Int
    , paste : Maybe ( String, String )
    , find : Maybe ( String, String )
    , ref : Maybe String
    , share : Maybe String
    , templates : Bool
    , slideshow : Bool
    , slide : Int
    , report : Bool
    , past : List Doc
    , future : List Doc
    , active : Maybe Int
    , folded : Set Int
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


{-| Turn off every alternate output view (chart / profile / pivot / correlation) for a cell, so the
output toggle behaves as one mutually-exclusive picker. -}
clearModes : Int -> NbDoc -> NbDoc
clearModes id nb =
    { nb
        | charts = Dict.remove id nb.charts
        , profiles = Set.remove id nb.profiles
        , pivots = Dict.remove id nb.pivots
        , corrs = Set.remove id nb.corrs
    }


{-| The value a cell currently shows, if it produced one. -}
cellValue : Int -> NbDoc -> Maybe Lang.Value
cellValue id nb =
    Doc.find id nb.doc
        |> Maybe.andThen
            (\c ->
                case c.output of
                    OutValue v ->
                        Just v

                    _ ->
                        Nothing
            )


{-| The ids of the cells strictly before the given cell (for "run above"). -}
cellIdsBefore : Int -> Doc -> Set Int
cellIdsBefore id doc =
    List.map .id doc.cells
        |> List.foldl
            (\i ( acc, stop ) ->
                if stop || i == id then
                    ( acc, True )

                else
                    ( i :: acc, False )
            )
            ( [], False )
        |> Tuple.first
        |> Set.fromList


{-| The ids of the given cell and every cell after it (for "run from here down"). -}
cellIdsFrom : Int -> Doc -> Set Int
cellIdsFrom id doc =
    List.map .id doc.cells
        |> List.foldl
            (\i ( acc, started ) ->
                if started || i == id then
                    ( i :: acc, True )

                else
                    ( acc, False )
            )
            ( [], False )
        |> Tuple.first
        |> Set.fromList


{-| How many presentation slides the notebook splits into. -}
slideCount : Doc -> Int
slideCount doc =
    List.length (Slides.slides doc)


{-| A starting pivot spec derived from a cell's table value (or an empty one). -}
pivotSpecFor : Int -> NbDoc -> Pivot.Spec
pivotSpecFor id nb =
    case cellValue id nb of
        Just value ->
            Pivot.defaultSpec value

        Nothing ->
            { row = "", column = "", value = "", agg = Pivot.Sum }


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
    | SetProfile Int Bool
    | SetPivot Int Bool
    | SetPivotRow Int String
    | SetPivotColumn Int String
    | SetPivotValue Int String
    | SetPivotAgg Int String
    | SetCorr Int Bool
    | ToggleHeat Int Bool
    | ToggleFooter Int Bool
    | OpenImport
    | SetImportName String
    | SetImportText String
    | DoImport
    | CancelImport
    | ToggleReport
    | Undo
    | Redo
    | OpenFind
    | SetFindQuery String
    | SetFindReplace String
    | ReplaceAll
    | CloseFind
    | OpenRef
    | SetRefQuery String
    | InsertSnippet String
    | CloseRef
    | ToggleFold Int
    | DuplicateCell Int
    | InsertAbove Int
    | InsertBelow Int
    | RunAbove Int
    | RunBelow Int
    | ToggleSlides
    | NextSlide
    | PrevSlide
    | OpenShare
    | SetShareInput String
    | LoadShared
    | CloseShare
    | OpenTemplates
    | CloseTemplates
    | LoadTemplate String
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
    D.map (\d -> { doc = d, carets = Dict.empty, charts = Dict.empty, cols = Dict.empty, tables = Dict.empty, profiles = Set.empty, pivots = Dict.empty, corrs = Set.empty, heats = Set.empty, footers = Set.empty, paste = Nothing, find = Nothing, ref = Nothing, share = Nothing, templates = False, slideshow = False, slide = 0, report = False, past = [], future = [], active = Nothing, folded = Set.empty, lesson = "", stale = Set.empty }) Serialize.decoder


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
    , profiles = Set.empty
    , pivots = Dict.empty
    , corrs = Set.empty
    , heats = Set.empty
    , footers = Set.empty
    , paste = Nothing
    , find = Nothing
    , ref = Nothing
    , share = Nothing
    , templates = False
    , slideshow = False
    , slide = 0
    , report = False
    , past = []
    , future = []
    , active = Nothing
    , folded = Set.empty
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
    , profiles = Set.empty
    , pivots = Dict.empty
    , corrs = Set.empty
    , heats = Set.empty
    , footers = Set.empty
    , paste = Nothing
    , find = Nothing
    , ref = Nothing
    , share = Nothing
    , templates = False
    , slideshow = False
    , slide = 0
    , report = False
    , past = []
    , future = []
    , active = Nothing
    , folded = Set.empty
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


{-| Structural / content changes worth an undo step (a snapshot of the document is pushed before
they run). Per-keystroke edits, runs, input-widget tweaks and pure view toggles aren't snapshotted —
they're cheap to redo, and a text edit has the textarea's own undo. -}
undoable : NbMsg -> Bool
undoable msg =
    case msg of
        DeleteCell _ ->
            True

        MoveUp _ ->
            True

        MoveDown _ ->
            True

        Convert _ _ ->
            True

        Insert _ ->
            True

        AddCode ->
            True

        AddMarkdown ->
            True

        AddInput ->
            True

        Clear ->
            True

        LoadLesson _ ->
            True

        Fix _ _ ->
            True

        DoImport ->
            True

        ReplaceAll ->
            True

        InsertSnippet _ ->
            True

        DuplicateCell _ ->
            True

        InsertAbove _ ->
            True

        InsertBelow _ ->
            True

        LoadTemplate _ ->
            True

        LoadShared ->
            True

        _ ->
            False


pushUndo : NbDoc -> NbDoc
pushUndo nb =
    { nb | past = nb.doc :: List.take 49 nb.past, future = [] }


updateNb : NbMsg -> NbDoc -> NbDoc
updateNb msg nb =
    step msg
        (if undoable msg then
            pushUndo nb

         else
            nb
        )


step : NbMsg -> NbDoc -> NbDoc
step msg nb =
    case msg of
        Undo ->
            case nb.past of
                prev :: rest ->
                    { nb | doc = prev, past = rest, future = nb.doc :: nb.future, stale = Set.empty }

                [] ->
                    nb

        Redo ->
            case nb.future of
                next :: rest ->
                    { nb | doc = next, future = rest, past = nb.doc :: nb.past, stale = Set.empty }

                [] ->
                    nb

        Edit id source caret ->
            let
                doc2 =
                    Doc.setSource id source nb.doc
            in
            -- the edited cell and everything downstream of it now need re-running
            { nb
                | doc = doc2
                , carets = Dict.insert id caret nb.carets
                , active = Just id
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
            let
                c =
                    clearModes id nb
            in
            case maybeKind of
                Just kind ->
                    { c | charts = Dict.insert id kind c.charts }

                Nothing ->
                    c

        SetCol id colName ->
            { nb | cols = Dict.insert id colName nb.cols }

        SetProfile id flag ->
            let
                c =
                    clearModes id nb
            in
            if flag then
                { c | profiles = Set.insert id c.profiles }

            else
                c

        SetPivot id flag ->
            let
                c =
                    clearModes id nb
            in
            if flag then
                { c | pivots = Dict.insert id (pivotSpecFor id nb) c.pivots }

            else
                c

        SetPivotRow id colName ->
            { nb | pivots = Dict.update id (Maybe.map (Pivot.withRow colName)) nb.pivots }

        SetPivotColumn id colName ->
            { nb | pivots = Dict.update id (Maybe.map (Pivot.withColumn colName)) nb.pivots }

        SetPivotValue id colName ->
            { nb | pivots = Dict.update id (Maybe.map (Pivot.withValue colName)) nb.pivots }

        SetPivotAgg id aggName ->
            { nb | pivots = Dict.update id (Maybe.map (Pivot.withAgg (Pivot.aggFromString aggName))) nb.pivots }

        SetCorr id flag ->
            let
                c =
                    clearModes id nb
            in
            if flag then
                { c | corrs = Set.insert id c.corrs }

            else
                c

        ToggleHeat id flag ->
            { nb
                | heats =
                    if flag then
                        Set.insert id nb.heats

                    else
                        Set.remove id nb.heats
            }

        ToggleFooter id flag ->
            { nb
                | footers =
                    if flag then
                        Set.insert id nb.footers

                    else
                        Set.remove id nb.footers
            }

        OpenImport ->
            { nb | paste = Just ( "data", "" ) }

        SetImportName name ->
            { nb | paste = Maybe.map (\( _, txt ) -> ( name, txt )) nb.paste }

        SetImportText txt ->
            { nb | paste = Maybe.map (\( name, _ ) -> ( name, txt )) nb.paste }

        CancelImport ->
            { nb | paste = Nothing }

        ToggleReport ->
            { nb | report = not nb.report }

        OpenFind ->
            { nb | find = Just ( "", "" ) }

        SetFindQuery q ->
            { nb | find = Maybe.map (\( _, r ) -> ( q, r )) nb.find }

        SetFindReplace r ->
            { nb | find = Maybe.map (\( q, _ ) -> ( q, r )) nb.find }

        CloseFind ->
            { nb | find = Nothing }

        OpenRef ->
            { nb | ref = Just "" }

        SetRefQuery q ->
            { nb | ref = Just q }

        CloseRef ->
            { nb | ref = Nothing }

        InsertSnippet snippet ->
            { nb | doc = nb.doc |> Doc.append Code snippet |> Doc.runAll }

        ToggleFold id ->
            { nb
                | folded =
                    if Set.member id nb.folded then
                        Set.remove id nb.folded

                    else
                        Set.insert id nb.folded
            }

        DuplicateCell id ->
            { nb | doc = Doc.duplicate id nb.doc |> Doc.runAll }

        InsertAbove id ->
            { nb | doc = Doc.insertBefore id Code "" nb.doc }

        InsertBelow id ->
            { nb | doc = Doc.insertAfter id Code "" nb.doc }

        RunAbove id ->
            { nb | doc = Doc.runAffected (cellIdsBefore id nb.doc) nb.doc, stale = nb.stale }

        RunBelow id ->
            { nb | doc = Doc.runAffected (cellIdsFrom id nb.doc) nb.doc, stale = nb.stale }

        ToggleSlides ->
            { nb | slideshow = not nb.slideshow, slide = 0 }

        NextSlide ->
            { nb | slide = Basics.min (slideCount nb.doc - 1) (nb.slide + 1) }

        PrevSlide ->
            { nb | slide = Basics.max 0 (nb.slide - 1) }

        OpenShare ->
            { nb | share = Just "" }

        SetShareInput token ->
            { nb | share = Just token }

        CloseShare ->
            { nb | share = Nothing }

        LoadShared ->
            case Maybe.andThen Share.decode (Maybe.map stripShareToken nb.share) of
                Just loaded ->
                    { nb | doc = Doc.runAll loaded, share = Nothing, stale = Set.empty }

                Nothing ->
                    nb

        OpenTemplates ->
            { nb | templates = True }

        CloseTemplates ->
            { nb | templates = False }

        LoadTemplate id ->
            case Templates.byId id of
                Just template ->
                    { nb | doc = Doc.fromSpec template.cells |> Doc.runAll, templates = False, stale = Set.empty }

                Nothing ->
                    { nb | templates = False }

        ReplaceAll ->
            case nb.find of
                Just ( q, r ) ->
                    if q == "" then
                        nb

                    else
                        let
                            doc2 =
                                List.foldl
                                    (\c d ->
                                        if String.contains q c.source then
                                            Doc.setSource c.id (String.replace q r c.source) d

                                        else
                                            d
                                    )
                                    nb.doc
                                    nb.doc.cells
                        in
                        { nb | doc = Doc.runAll doc2, stale = Set.empty }

                Nothing ->
                    nb

        DoImport ->
            case nb.paste of
                Just ( name, txt ) ->
                    let
                        source =
                            case Import.toElm name txt of
                                Ok src ->
                                    src

                                Err message ->
                                    "-- import failed: " ++ message
                    in
                    { nb | doc = nb.doc |> Doc.append Code source |> Doc.runAll, paste = Nothing }

                Nothing ->
                    nb

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
    if nb.slideshow then
        slideshowView env nb

    else
        editView showCopy env nb


editView : Bool -> Workspace.EditorEnv -> NbDoc -> Html NbMsg
editView showCopy env nb =
    div
        [ HA.class
            ("nb-workspace"
                ++ (if nb.report then
                        " nb-report-mode"

                    else
                        ""
                   )
            )
        ]
        [ if nb.report then
            text ""

          else
            lessonBar nb.lesson
        , toolbar showCopy (Set.size nb.stale) nb.report (not (List.isEmpty nb.past)) (not (List.isEmpty nb.future)) nb.doc
        , findBar nb.find nb.doc
        , refPanel nb.ref
        , sharePanel nb.share nb.doc
        , templatePanel nb.templates
        , pastePanel nb.paste
        , section [ HA.class "nb-main" ]
            [ div [ HA.class "nb-notebook" ]
                [ NbView.notebook (viewConfig env nb) nb.doc
                , if nb.report then
                    text ""

                  else
                    toolbar showCopy (Set.size nb.stale) nb.report (not (List.isEmpty nb.past)) (not (List.isEmpty nb.future)) nb.doc
                ]
            , if nb.report then
                text ""

              else
                div [ HA.class "nb-sidebar" ]
                    [ NbView.errorsPanel Run (errorList nb.doc)
                    , NbView.outlinePanel (Outline.headings nb.doc)
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
    , evalInline = evalInline nb
    , isProfile = \id -> Set.member id nb.profiles
    , onProfile = SetProfile
    , pivotOf = \id -> Dict.get id nb.pivots
    , onPivot = SetPivot
    , onPivotRow = SetPivotRow
    , onPivotColumn = SetPivotColumn
    , onPivotValue = SetPivotValue
    , onPivotAgg = SetPivotAgg
    , isCorr = \id -> Set.member id nb.corrs
    , onCorr = SetCorr
    , report = nb.report
    , activeCell = nb.active
    , scopeNames = Kernel.names nb.doc.kernel
    , findQuery = Maybe.map Tuple.first nb.find
    , isFolded = \id -> Set.member id nb.folded
    , onFold = ToggleFold
    , onDuplicate = DuplicateCell
    , onInsertAbove = InsertAbove
    , onInsertBelow = InsertBelow
    , onRunAbove = RunAbove
    , onRunBelow = RunBelow
    , heatOn = \id -> Set.member id nb.heats
    , onHeat = ToggleHeat
    , footerOn = \id -> Set.member id nb.footers
    , onFooter = ToggleFooter
    }


{-| Evaluate a `{{ expr }}` from a markdown cell against the current kernel, for inline display. -}
evalInline : NbDoc -> String -> Maybe String
evalInline nb expr =
    if String.trim expr == "" then
        Nothing

    else
        case Tuple.first (Kernel.run expr nb.doc.kernel) of
            OutValue value ->
                Just (Value.displayValue value)

            OutError message ->
                Just ("⚠ " ++ message)

            OutNone ->
                Nothing


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


reportToggle : Bool -> Html NbMsg
reportToggle report =
    button [ HA.class "nb-action nb-action-report", HE.onClick ToggleReport ]
        [ Html.i [ HA.class ("bi " ++ iconFor report) ] []
        , text
            (if report then
                " Edit"

             else
                " Report"
            )
        ]


iconFor : Bool -> String
iconFor report =
    if report then
        "bi-pencil"

    else
        "bi-easel"


undoButtons : Bool -> Bool -> List (Html NbMsg)
undoButtons canUndo canRedo =
    [ button [ HA.class "nb-action nb-action-icon", HA.disabled (not canUndo), HA.title "Undo", HE.onClick Undo ]
        [ Html.i [ HA.class "bi bi-arrow-counterclockwise" ] [] ]
    , button [ HA.class "nb-action nb-action-icon", HA.disabled (not canRedo), HA.title "Redo", HE.onClick Redo ]
        [ Html.i [ HA.class "bi bi-arrow-clockwise" ] [] ]
    ]


toolbar : Bool -> Int -> Bool -> Bool -> Bool -> Doc -> Html NbMsg
toolbar showCopy staleCount report canUndo canRedo doc =
    if report then
        section [ HA.class "nb-actions" ]
            [ button [ HA.class "nb-action nb-action-primary", HE.onClick RunAll ] [ text "▶▶ Run all" ]
            , reportToggle report
            ]

    else
        section [ HA.class "nb-actions" ]
        ([ button [ HA.class "nb-action nb-action-primary", HE.onClick RunAll ] [ text "▶▶ Run all" ] ]
            ++ undoButtons canUndo canRedo
            ++ [ if staleCount > 0 then
            button [ HA.class "nb-action nb-action-stale", HE.onClick RunStale, HA.title "Re-run the cells affected by your edits" ]
                [ Html.i [ HA.class "bi bi-arrow-repeat" ] [], text (" Run stale (" ++ String.fromInt staleCount ++ ")") ]

          else
            text ""
        , button [ HA.class "nb-action", HE.onClick AddCode ] [ text "+ Code cell" ]
        , button [ HA.class "nb-action", HE.onClick AddMarkdown ] [ text "+ Text cell" ]
        , button [ HA.class "nb-action", HE.onClick AddInput ] [ text "+ Input" ]
        , button [ HA.class "nb-action", HE.onClick OpenImport ] [ Html.i [ HA.class "bi bi-clipboard-data" ] [], text " Import data" ]
        , button [ HA.class "nb-action", HE.onClick OpenFind ] [ Html.i [ HA.class "bi bi-search" ] [], text " Find" ]
        , button [ HA.class "nb-action", HE.onClick OpenRef ] [ Html.i [ HA.class "bi bi-journal-code" ] [], text " Reference" ]
        , button [ HA.class "nb-action", HE.onClick OpenTemplates ] [ Html.i [ HA.class "bi bi-grid-1x2" ] [], text " Templates" ]
        , button [ HA.class "nb-action", HE.onClick ToggleSlides ] [ Html.i [ HA.class "bi bi-easel2" ] [], text " Slides" ]
        , button [ HA.class "nb-action", HE.onClick OpenShare ] [ Html.i [ HA.class "bi bi-share" ] [], text " Share" ]
        , button [ HA.class "nb-action", HE.onClick Clear ] [ text "Clear outputs" ]
        , span [ HA.class "nb-action-export" ] [ Export.notebookLinks doc ]
        , reportToggle report
        , if showCopy then
            button [ HA.class "nb-action nb-action-copy", HE.onClick CopyToWorkspaceRequested ]
                [ Html.i [ HA.class "bi bi-folder-plus" ] [], text " Copy to workspace" ]

          else
            text ""
         ]
        )


{-| The function reference: a search box over the prelude + stdlib catalog; clicking an entry appends
a code cell with its snippet. -}
refPanel : Maybe String -> Html NbMsg
refPanel ref =
    case ref of
        Nothing ->
            text ""

        Just query ->
            section [ HA.class "nb-ref" ]
                [ div [ HA.class "nb-ref-head" ]
                    [ Html.i [ HA.class "bi bi-journal-code" ] []
                    , Html.input
                        [ HA.class "nb-ref-search", HA.placeholder "Search functions…", HA.value query, HA.attribute "spellcheck" "false", HE.onInput SetRefQuery ]
                        []
                    , button [ HA.class "nb-action nb-action-icon", HA.title "Close", HE.onClick CloseRef ] [ Html.i [ HA.class "bi bi-x" ] [] ]
                    ]
                , div [ HA.class "nb-ref-list" ] (List.map refEntry (Reference.search query))
                ]


refEntry : Reference.Entry -> Html NbMsg
refEntry entry =
    button [ HA.class "nb-ref-item", HA.title (entry.name ++ " : " ++ entry.signature), HE.onClick (InsertSnippet entry.snippet) ]
        [ span [ HA.class "nb-ref-name" ] [ text entry.name ]
        , span [ HA.class "nb-ref-doc" ] [ text entry.doc ]
        ]


{-| The find / replace bar: a query + replacement, a live count of matching cells, and replace-all. -}
findBar : Maybe ( String, String ) -> Doc -> Html NbMsg
findBar find doc =
    case find of
        Nothing ->
            text ""

        Just ( q, r ) ->
            let
                count =
                    List.length (List.filter (\c -> q /= "" && String.contains q c.source) doc.cells)
            in
            section [ HA.class "nb-find" ]
                [ Html.i [ HA.class "bi bi-search" ] []
                , Html.input
                    [ HA.class "nb-find-input", HA.placeholder "Find in cells…", HA.value q, HA.attribute "spellcheck" "false", HE.onInput SetFindQuery ]
                    []
                , Html.input
                    [ HA.class "nb-find-input", HA.placeholder "Replace with…", HA.value r, HA.attribute "spellcheck" "false", HE.onInput SetFindReplace ]
                    []
                , span [ HA.class "nb-find-count" ]
                    [ text
                        (if q == "" then
                            ""

                         else
                            String.fromInt count
                                ++ (if count == 1 then
                                        " cell"

                                    else
                                        " cells"
                                   )
                        )
                    ]
                , button [ HA.class "nb-action", HE.onClick ReplaceAll ] [ text "Replace all" ]
                , button [ HA.class "nb-action nb-action-icon", HA.title "Close", HE.onClick CloseFind ] [ Html.i [ HA.class "bi bi-x" ] [] ]
                ]


{-| The "Paste data" panel: a name + a textarea that auto-detects JSON vs CSV/TSV on import. -}
pastePanel : Maybe ( String, String ) -> Html NbMsg
pastePanel paste =
    case paste of
        Nothing ->
            text ""

        Just ( name, txt ) ->
            section [ HA.class "nb-import" ]
                [ div [ HA.class "nb-import-head" ]
                    [ span [ HA.class "nb-import-title" ] [ text "Paste data — a JSON array of objects, or CSV / TSV" ]
                    , Html.input
                        [ HA.class "nb-import-name", HA.value name, HA.placeholder "name", HA.attribute "spellcheck" "false", HE.onInput SetImportName ]
                        []
                    ]
                , Html.textarea
                    [ HA.class "nb-import-text"
                    , HA.attribute "rows" "6"
                    , HA.attribute "spellcheck" "false"
                    , HA.value txt
                    , HA.placeholder "[ { \"name\": \"Ada\", \"age\": 36 }, … ]   or   name,age\\nAda,36"
                    , HE.onInput SetImportText
                    ]
                    []
                , div [ HA.class "nb-import-actions" ]
                    [ button [ HA.class "nb-action nb-action-primary", HE.onClick DoImport ] [ text "Import → cell" ]
                    , button [ HA.class "nb-action", HE.onClick CancelImport ] [ text "Cancel" ]
                    , span [ HA.class "nb-import-hint" ]
                        [ text
                            (if String.trim txt == "" then
                                ""

                             else if Import.looksLikeJson txt then
                                "detected: JSON"

                             else
                                "detected: CSV / TSV"
                            )
                        ]
                    ]
                ]


{-| Presentation mode: one slide at a time — its cells rendered live — with prev/next navigation, a
position indicator, and an "Edit" button back to the notebook. -}
slideshowView : Workspace.EditorEnv -> NbDoc -> Html NbMsg
slideshowView env nb =
    let
        deck =
            Slides.slides nb.doc

        total =
            List.length deck

        idx =
            Basics.max 0 (Basics.min (total - 1) nb.slide)

        current =
            List.drop idx deck |> List.head
    in
    div [ HA.class "nb-slideshow" ]
        [ section [ HA.class "nb-slide-bar" ]
            [ button [ HA.class "nb-action", HA.disabled (idx <= 0), HE.onClick PrevSlide ]
                [ Html.i [ HA.class "bi bi-chevron-left" ] [], text " Prev" ]
            , span [ HA.class "nb-slide-pos" ]
                [ text (String.fromInt (idx + 1) ++ " / " ++ String.fromInt (Basics.max 1 total)) ]
            , button [ HA.class "nb-action", HA.disabled (idx >= total - 1), HE.onClick NextSlide ]
                [ text "Next ", Html.i [ HA.class "bi bi-chevron-right" ] [] ]
            , button [ HA.class "nb-action nb-action-report", HE.onClick ToggleSlides ]
                [ Html.i [ HA.class "bi bi-pencil" ] [], text " Edit" ]
            ]
        , case current of
            Just slide ->
                div [ HA.class "nb-slide" ]
                    [ NbView.notebook (viewConfig env nb) (Doc.withCells slide.cells nb.doc) ]

            Nothing ->
                div [ HA.class "nb-slide nb-slide-empty" ]
                    [ text "Add a “# heading” to a text cell to start a slide." ]
        ]


{-| Share-by-link: the current notebook encoded as a copyable token, plus a box to paste a token (or
a `#nb=…` link) and load it over the current notebook. -}
sharePanel : Maybe String -> Doc -> Html NbMsg
sharePanel share doc =
    case share of
        Nothing ->
            text ""

        Just token ->
            section [ HA.class "nb-share" ]
                [ div [ HA.class "nb-share-head" ]
                    [ span [ HA.class "nb-share-title" ] [ Html.i [ HA.class "bi bi-share" ] [], text " Share this notebook" ]
                    , button [ HA.class "nb-action nb-action-icon", HA.title "Close", HE.onClick CloseShare ] [ Html.i [ HA.class "bi bi-x" ] [] ]
                    ]
                , span [ HA.class "nb-share-label" ] [ text "Copy this link — it carries the whole notebook:" ]
                , Html.input
                    [ HA.class "nb-share-link", HA.attribute "readonly" "readonly", HA.value (Share.link "" doc), HA.attribute "spellcheck" "false" ]
                    []
                , span [ HA.class "nb-share-label" ] [ text "…or paste a shared link / token to load it:" ]
                , Html.input
                    [ HA.class "nb-share-input", HA.placeholder "#nb=… or token", HA.value token, HA.attribute "spellcheck" "false", HE.onInput SetShareInput ]
                    []
                , div [ HA.class "nb-import-actions" ]
                    [ button [ HA.class "nb-action nb-action-primary", HE.onClick LoadShared ] [ text "Load shared notebook" ]
                    , button [ HA.class "nb-action", HE.onClick CloseShare ] [ text "Cancel" ]
                    ]
                ]


{-| The "New from template" picker: each starter template as a card that replaces the notebook's
cells when chosen. -}
templatePanel : Bool -> Html NbMsg
templatePanel open =
    if not open then
        text ""

    else
        section [ HA.class "nb-templates" ]
            [ div [ HA.class "nb-templates-head" ]
                [ span [ HA.class "nb-templates-title" ] [ Html.i [ HA.class "bi bi-grid-1x2" ] [], text " New from template" ]
                , button [ HA.class "nb-action nb-action-icon", HA.title "Close", HE.onClick CloseTemplates ] [ Html.i [ HA.class "bi bi-x" ] [] ]
                ]
            , div [ HA.class "nb-templates-list" ] (List.map templateCard Templates.all)
            ]


templateCard : Templates.Template -> Html NbMsg
templateCard t =
    button [ HA.class "nb-template-card", HE.onClick (LoadTemplate t.id) ]
        [ span [ HA.class "nb-template-name" ] [ text t.title ]
        , span [ HA.class "nb-template-blurb" ] [ text t.blurb ]
        ]


{-| Reduce a pasted share value — a full `#nb=…` link or a bare token — to just the token. -}
stripShareToken : String -> String
stripShareToken raw =
    let
        t =
            String.trim raw
    in
    case String.indexes "nb=" t of
        i :: _ ->
            String.dropLeft (i + 3) t

        [] ->
            t


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
