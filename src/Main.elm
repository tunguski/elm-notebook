module Main exposing (main)

{-| The elm-notebook site — a Jupyter-style notebook for exploring data in Elm, in the
browser.

A single `Browser.element` app drives one live notebook: editable code/markdown cells, a
stateful kernel that threads bindings from cell to cell, and a panel of context-aware
**suggested next steps** that reads the last result and proposes what to try. A row of
one-click **lessons** loads guided notebooks so a newcomer can learn by running and tweaking
real cells rather than reading docs.

All notebook logic lives in `Notebook.*`; this module only wires the document to the view
and the toolbar.

-}

import Browser
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { doc = Doc.fromSpec Suggest.starter |> Doc.runAll
      , lesson = "starter"
      }
    , Cmd.none
    )



-- UPDATE ---------------------------------------------------------------------


type Msg
    = Edit Int String
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
        Edit id source ->
            { model | doc = Doc.setSource id source model.doc }

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
            }



-- VIEW -----------------------------------------------------------------------


viewConfig : View.Config Msg
viewConfig =
    { onEdit = Edit
    , onRun = Run
    , onDelete = Delete
    , onMoveUp = MoveUp
    , onMoveDown = MoveDown
    , onConvert = Convert
    , onInsert = Insert
    }


view : Model -> Html Msg
view model =
    div [ HA.class "nb-app" ]
        [ pageHeader
        , lessonBar model.lesson
        , toolbar
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
                [ text "A Jupyter-style notebook for exploring data in "
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


toolbar : Html Msg
toolbar =
    section [ HA.class "nb-actions" ]
        [ button [ HA.class "nb-action nb-action-primary", HE.onClick RunAll ] [ text "▶▶ Run all" ]
        , button [ HA.class "nb-action", HE.onClick AddCode ] [ text "+ Code cell" ]
        , button [ HA.class "nb-action", HE.onClick AddMarkdown ] [ text "+ Text cell" ]
        , button [ HA.class "nb-action", HE.onClick Clear ] [ text "Clear outputs" ]
        ]


main_ : Model -> Html Msg
main_ model =
    section [ HA.class "nb-main" ]
        [ div [ HA.class "nb-notebook" ]
            [ View.notebook viewConfig model.doc ]
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
            [ Html.li [] [ text "Each code cell is one Elm-flavoured expression." ]
            , Html.li [] [ text "Write name = expr to reuse a result in later cells." ]
            , Html.li [] [ text "_ always refers to the previous result." ]
            , Html.li [] [ text "A list of records renders as a table." ]
            , Html.li [] [ text "Functions: map, filter, foldl, column, groupBy, mean, sortByField…" ]
            ]
        ]


pageFooter : Html Msg
pageFooter =
    footer [ HA.class "nb-foot" ]
        [ div []
            [ text "elm-notebook — built on the "
            , a [ HA.href "https://github.com/tunguski/elm-lang" ] [ text "elm-lang" ]
            , text " compiler."
            ]
        , div [ HA.class "nb-foot-links" ]
            [ a [ HA.href "tests.html" ] [ text "Test report" ]
            , a [ HA.href "https://github.com/tunguski/elm-notebook" ] [ text "GitHub" ]
            , a [ HA.href "https://tunguski.github.io/" ] [ text "More projects" ]
            ]
        ]
