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
import Notebook.Doc as Doc exposing (Doc)
import Notebook.Cell as Cell exposing (Cell, CellKind(..), Control(..))
import Notebook.Export as Export
import Notebook.Serialize as Serialize
import Notebook.Suggest as Suggest exposing (Lesson, Suggestion)
import Notebook.View as NbView
import Workspace
import Workspace.Table as Table
import Workspace.Types exposing (Table)


{-| A notebook document plus its transient editor state. -}
type alias NbDoc =
    { doc : Doc
    , carets : Dict Int Int
    , charts : Dict Int Chart.ChartKind
    , lesson : String
    }


{-| The notebook editor's messages (everything the old single-notebook `Main` handled). -}
type NbMsg
    = Edit Int String Int
    | Run Int
    | RunAll
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
    D.map (\d -> { doc = d, carets = Dict.empty, charts = Dict.empty, lesson = "" }) Serialize.decoder


empty : NbDoc
empty =
    { doc =
        Doc.empty
            |> Doc.append Markdown "# New notebook\n\nDescribe what you're exploring, then add a code cell."
            |> Doc.append Code ""
            |> Doc.runAll
    , carets = Dict.empty
    , charts = Dict.empty
    , lesson = ""
    }


{-| The starter notebook used by the standalone playground (the site's "examples" landing) — the
guided starter run and ready to explore, without any workspace chrome. -}
examples : NbDoc
examples =
    { doc = Doc.fromSpec Suggest.starter |> Doc.runAll
    , carets = Dict.empty
    , charts = Dict.empty
    , lesson = "starter"
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
            { nb | doc = Doc.setSource id source nb.doc, carets = Dict.insert id caret nb.carets }

        Run id ->
            { nb | doc = Doc.runThrough id nb.doc }

        RunAll ->
            { nb | doc = Doc.runAll nb.doc }

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
            { nb | doc = Doc.clearOutputs nb.doc }

        LoadLesson lesson ->
            { nb | doc = Doc.fromSpec lesson.cells |> Doc.runAll, lesson = lesson.id, carets = Dict.empty, charts = Dict.empty }

        SetChart id maybeKind ->
            { nb
                | charts =
                    case maybeKind of
                        Just kind ->
                            Dict.insert id kind nb.charts

                        Nothing ->
                            Dict.remove id nb.charts
            }

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
        , toolbar showCopy
        , section [ HA.class "nb-main" ]
            [ div [ HA.class "nb-notebook" ]
                [ NbView.notebook (viewConfig env nb) nb.doc
                , toolbar showCopy
                ]
            , div [ HA.class "nb-sidebar" ]
                [ NbView.suggestionsPanel Insert (Suggest.suggestNext (Doc.lastValue nb.doc))
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
    , onInputValue = SetInputValue
    , onInputName = SetInputName
    , onInputControl = SetInputControl
    , commentsVisible = env.commentsVisible
    , commentCountOf = \id -> env.commentCount (String.fromInt id)
    , exportCell = \cell -> exportCell cell
    }


exportCell : Cell -> Html NbMsg
exportCell cell =
    case cell.output of
        Cell.OutValue value ->
            Export.cellLinks "step" value

        _ ->
            text ""


toolbar : Bool -> Html NbMsg
toolbar showCopy =
    section [ HA.class "nb-actions" ]
        [ button [ HA.class "nb-action nb-action-primary", HE.onClick RunAll ] [ text "▶▶ Run all" ]
        , button [ HA.class "nb-action", HE.onClick AddCode ] [ text "+ Code cell" ]
        , button [ HA.class "nb-action", HE.onClick AddMarkdown ] [ text "+ Text cell" ]
        , button [ HA.class "nb-action", HE.onClick AddInput ] [ text "+ Input" ]
        , button [ HA.class "nb-action", HE.onClick Clear ] [ text "Clear outputs" ]
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
