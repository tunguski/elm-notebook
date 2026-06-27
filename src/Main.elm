module Main exposing (main)

{-| The elm-notebook site — a Jupyter-style notebook for exploring data in **real Elm**, in the
browser.

A single `Browser.element` app drives one live notebook: syntax-highlighted, auto-growing cells,
a stateful kernel (the vendored elm-in-elm interpreter) that threads definitions from cell to
cell, and a panel of context-aware **suggested next steps** that reads the last result. A row of
one-click **lessons** loads guided notebooks. Toolbars at the top *and* foot of the notebook give
quick access to Run all / add cell / clear.

All notebook logic lives in `Notebook.*` (over the vendored `Lang`/`Lexer`/`Parser`/`Eval`); this
module only wires the document to the view.

-}

import Browser
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, footer, h1, header, input, p, section, span, text, textarea)
import Html.Attributes as HA
import Html.Events as HE
import Notebook.Cell as Cell exposing (CellKind(..), Control(..))
import Notebook.Chart as Chart
import Notebook.Csv as Csv
import Notebook.Doc as Doc exposing (Doc)
import Notebook.Serialize as Serialize
import Notebook.Suggest as Suggest exposing (Lesson, Suggestion)
import Notebook.View as View
import Storage


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- MODEL ----------------------------------------------------------------------


type alias Model =
    { doc : Doc
    , lesson : String
    , carets : Dict Int Int
    , charts : Dict Int Chart.ChartKind
    , csvOpen : Bool
    , csvName : String
    , csvText : String
    , csvError : Maybe String
    }


storageKey : String
storageKey =
    "elm-notebook:autosave"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { doc = Doc.fromSpec Suggest.starter |> Doc.runAll
      , lesson = "starter"
      , carets = Dict.empty
      , charts = Dict.empty
      , csvOpen = False
      , csvName = "data"
      , csvText = ""
      , csvError = Nothing
      }
      -- restore the autosaved notebook, if any
    , Storage.load storageKey Loaded
    )



-- UPDATE ---------------------------------------------------------------------


type Msg
    = Edit Int String Int
    | Run Int
    | RunAll
    | Delete Int
    | MoveUp Int
    | MoveDown Int
    | Convert Int CellKind
    | Insert Suggestion
    | AddCode
    | AddMarkdown
    | AddInput
    | Clear
    | NewNotebook
    | Loaded (Maybe String)
    | LoadLesson Lesson
    | SetChart Int (Maybe Chart.ChartKind)
    | InsertName String
    | SetInputValue Int String
    | SetInputName Int String
    | SetInputControl Int String
    | ToggleCsv
    | SetCsvName String
    | SetCsvText String
    | ImportCsv


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updated =
            pureUpdate msg model
    in
    -- autosave the notebook to local storage after every change
    ( updated, Storage.save storageKey (Serialize.encode updated.doc) )


pureUpdate : Msg -> Model -> Model
pureUpdate msg model =
    case msg of
        Edit id source caret ->
            { model
                | doc = Doc.setSource id source model.doc
                , carets = Dict.insert id caret model.carets
            }

        Run id ->
            { model | doc = Doc.runThrough id model.doc }

        RunAll ->
            { model | doc = Doc.runAll model.doc }

        Delete id ->
            { model | doc = Doc.remove id model.doc }

        MoveUp id ->
            { model | doc = Doc.moveUp id model.doc }

        MoveDown id ->
            { model | doc = Doc.moveDown id model.doc }

        Convert id kind ->
            { model | doc = Doc.setKind id kind model.doc }

        Insert suggestion ->
            { model
                | doc =
                    model.doc
                        |> Doc.append suggestion.kind suggestion.source
                        |> Doc.runAll
            }

        AddCode ->
            { model | doc = Doc.append Code "" model.doc }

        AddMarkdown ->
            { model | doc = Doc.append Markdown "## Notes\n\n…" model.doc }

        AddInput ->
            { model | doc = Doc.appendInput defaultInput model.doc |> Doc.runAll }

        SetInputValue id value ->
            { model | doc = Doc.setInputValue id value model.doc |> Doc.runAll }

        SetInputName id name ->
            { model | doc = Doc.setInputName id name model.doc |> Doc.runAll }

        SetInputControl id controlName ->
            { model | doc = Doc.setInputControl id (parseControl controlName) model.doc |> Doc.runAll }

        ToggleCsv ->
            { model | csvOpen = not model.csvOpen, csvError = Nothing }

        SetCsvName name ->
            { model | csvName = name }

        SetCsvText csv ->
            { model | csvText = csv }

        ImportCsv ->
            case Csv.toElm model.csvName model.csvText of
                Ok source ->
                    { model
                        | doc = model.doc |> Doc.append Code source |> Doc.runAll
                        , csvOpen = False
                        , csvText = ""
                        , csvError = Nothing
                    }

                Err message ->
                    { model | csvError = Just message }

        Clear ->
            { model | doc = Doc.clearOutputs model.doc }

        NewNotebook ->
            { model
                | doc = Doc.empty |> Doc.append Markdown "# New notebook\n\n…" |> Doc.append Code "" |> Doc.runAll
                , lesson = ""
                , carets = Dict.empty
                , charts = Dict.empty
            }

        Loaded maybeJson ->
            case maybeJson of
                Just json ->
                    case Serialize.decode json of
                        Ok doc ->
                            { model
                                | doc = Doc.runAll doc
                                , lesson = ""
                                , carets = Dict.empty
                                , charts = Dict.empty
                            }

                        Err _ ->
                            model

                Nothing ->
                    -- nothing saved yet
                    model

        LoadLesson lesson ->
            { model
                | doc = Doc.fromSpec lesson.cells |> Doc.runAll
                , lesson = lesson.id
                , carets = Dict.empty
                , charts = Dict.empty
            }

        InsertName name ->
            { model | doc = model.doc |> Doc.append Code name |> Doc.runAll }

        SetChart id maybeKind ->
            { model
                | charts =
                    case maybeKind of
                        Just kind ->
                            Dict.insert id kind model.charts

                        Nothing ->
                            Dict.remove id model.charts
            }


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


viewConfig : Model -> View.Config Msg
viewConfig model =
    { onEdit = Edit
    , onRun = Run
    , onDelete = Delete
    , onMoveUp = MoveUp
    , onMoveDown = MoveDown
    , onConvert = Convert
    , onInsert = Insert
    , caretOf = \id -> Dict.get id model.carets |> Maybe.withDefault 0
    , chartOf = \id -> Dict.get id model.charts
    , onChart = SetChart
    , onInputValue = SetInputValue
    , onInputName = SetInputName
    , onInputControl = SetInputControl
    }


view : Model -> Html Msg
view model =
    div [ HA.class "nb-app" ]
        [ pageHeader
        , lessonBar model.lesson
        , toolbar "nb-actions"
        , csvPanel model
        , main_ model
        , pageFooter
        ]


csvPanel : Model -> Html Msg
csvPanel model =
    if not model.csvOpen then
        text ""

    else
        section [ HA.class "nb-csv" ]
            [ div [ HA.class "nb-csv-head" ]
                [ span [ HA.class "nb-csv-title" ] [ text "Import CSV / TSV" ]
                , span [ HA.class "nb-csv-hint" ] [ text "Paste a spreadsheet export — a header row, then data. It becomes a List of records." ]
                ]
            , div [ HA.class "nb-csv-row" ]
                [ span [ HA.class "nb-csv-label" ] [ text "Name:" ]
                , input [ HA.class "nb-input-name", HA.value model.csvName, HE.onInput SetCsvName ] []
                , button [ HA.class "nb-action nb-action-primary", HE.onClick ImportCsv ] [ text "Add table" ]
                , button [ HA.class "nb-action", HE.onClick ToggleCsv ] [ text "Cancel" ]
                ]
            , textarea
                [ HA.class "nb-csv-text"
                , HA.attribute "rows" "7"
                , HA.placeholder "name, age, city\nAda, 36, Oslo\nGrace, 41, London"
                , HA.value model.csvText
                , HE.onInput SetCsvText
                ]
                []
            , case model.csvError of
                Just err ->
                    div [ HA.class "nb-csv-error" ] [ text err ]

                Nothing ->
                    text ""
            ]


pageHeader : Html Msg
pageHeader =
    header [ HA.class "nb-hero" ]
        [ div [ HA.class "nb-hero-inner" ]
            [ span [ HA.class "nb-eyebrow" ] [ text "elm · data exploration" ]
            , h1 [] [ text "elm-notebook" ]
            , p [ HA.class "nb-lead" ]
                [ text "A Jupyter-style notebook for exploring data in real "
                , a [ HA.href "https://elm-lang.org" ] [ text "Elm" ]
                , text ". Edit a cell, press Run, and build an analysis step by step — the app "
                , text "suggests where to go next so learning happens by doing."
                ]
            ]
        ]


lessonBar : String -> Html Msg
lessonBar active =
    section [ HA.class "nb-lessons" ]
        [ span [ HA.class "nb-lessons-label" ] [ text "Guided lessons:" ]
        , div [ HA.class "nb-lesson-buttons" ]
            (List.map (lessonButton active) Suggest.lessons)
        ]


lessonButton : String -> Lesson -> Html Msg
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


toolbar : String -> Html Msg
toolbar extraClass =
    section [ HA.class ("nb-actions " ++ extraClass) ]
        [ button [ HA.class "nb-action nb-action-primary", HE.onClick RunAll ] [ text "▶▶ Run all" ]
        , button [ HA.class "nb-action", HE.onClick AddCode ] [ text "+ Code cell" ]
        , button [ HA.class "nb-action", HE.onClick AddMarkdown ] [ text "+ Text cell" ]
        , button [ HA.class "nb-action", HE.onClick AddInput ] [ text "+ Input" ]
        , button [ HA.class "nb-action", HE.onClick ToggleCsv ] [ text "Import CSV" ]
        , button [ HA.class "nb-action", HE.onClick Clear ] [ text "Clear outputs" ]
        , button [ HA.class "nb-action", HE.onClick NewNotebook ] [ text "New" ]
        ]


main_ : Model -> Html Msg
main_ model =
    section [ HA.class "nb-main" ]
        [ div [ HA.class "nb-notebook" ]
            [ View.notebook (viewConfig model) model.doc
            , toolbar "nb-actions-bottom"
            ]
        , div [ HA.class "nb-sidebar" ]
            [ View.suggestionsPanel Insert (Suggest.suggestNext (Doc.lastValue model.doc))
            , View.variablesPanel InsertName (Doc.variables model.doc)
            , helpCard
            ]
        ]


helpCard : Html Msg
helpCard =
    div [ HA.class "nb-help" ]
        [ Html.h3 [ HA.class "nb-help-title" ] [ text "How it works" ]
        , Html.ul [ HA.class "nb-help-list" ]
            [ Html.li [] [ text "Each code cell is one real-Elm expression." ]
            , Html.li [] [ text "Write name = expr to reuse a result in later cells." ]
            , Html.li [] [ text "_ always refers to the previous result." ]
            , Html.li [] [ text "Press Shift+Enter (or Ctrl/Cmd+Enter) to run a cell." ]
            , Html.li [] [ text "A List of records renders as a table; toggle it to a chart." ]
            , Html.li [] [ text "The full List / String / Dict libraries are available, plus mean, groupBy and describe." ]
            , Html.li [] [ text "Your notebook autosaves to this browser and restores on return." ]
            ]
        ]


pageFooter : Html Msg
pageFooter =
    footer [ HA.class "nb-foot" ]
        [ div []
            [ text "elm-notebook — runs real Elm via the "
            , a [ HA.href "https://github.com/tunguski/elm-lang" ] [ text "elm-lang" ]
            , text " interpreter."
            ]
        , div [ HA.class "nb-foot-links" ]
            [ a [ HA.href "tests.html" ] [ text "Test report" ]
            , a [ HA.href "https://github.com/tunguski/elm-notebook" ] [ text "GitHub" ]
            , a [ HA.href "https://tunguski.github.io/" ] [ text "More projects" ]
            ]
        ]
