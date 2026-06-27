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
import Html exposing (Html, a, button, div, footer, h1, header, p, section, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Notebook.Cell exposing (CellKind(..))
import Notebook.Doc as Doc exposing (Doc)
import Notebook.Suggest as Suggest exposing (Lesson, Suggestion)
import Notebook.View as View


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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { doc = Doc.fromSpec Suggest.starter |> Doc.runAll
      , lesson = "starter"
      , carets = Dict.empty
      }
    , Cmd.none
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
    | Clear
    | LoadLesson Lesson


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( pureUpdate msg model, Cmd.none )


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

        Clear ->
            { model | doc = Doc.clearOutputs model.doc }

        LoadLesson lesson ->
            { model
                | doc = Doc.fromSpec lesson.cells |> Doc.runAll
                , lesson = lesson.id
                , carets = Dict.empty
            }



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
    }


view : Model -> Html Msg
view model =
    div [ HA.class "nb-app" ]
        [ pageHeader
        , lessonBar model.lesson
        , toolbar "nb-actions"
        , main_ model
        , pageFooter
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
        , button [ HA.class "nb-action", HE.onClick Clear ] [ text "Clear outputs" ]
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
            , Html.li [] [ text "A List of records renders as a table." ]
            , Html.li [] [ text "The full List / String / Dict libraries are available, plus mean and groupBy." ]
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
