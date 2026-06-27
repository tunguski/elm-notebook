module Main exposing (main)

{-| The elm-notebook site — a **workspace** of Jupyter-style notebooks that run real Elm in the
browser.

This module is a thin host: it wires the reusable [`Workspace`](Workspace) component to a browser
(localStorage) backend and a local user, and hands it the notebook document configuration from
[`Notebook.Workspace`](Notebook-Workspace). The workspace adds many-document management, naming,
search, copy, sharing/permissions, comments, URL import, SQL and export; the notebook itself (the
stateful kernel, highlighted cells, suggestions, variables, input widgets, charts) lives under
`Notebook.*`.

-}

import Browser
import Html exposing (Html, a, div, footer, h1, header, p, span, text)
import Html.Attributes as HA
import Notebook.Workspace as NB exposing (NbDoc, NbMsg)
import Workspace
import Workspace.Backend exposing (Backend, Context)
import Workspace.Browser


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- WIRING ---------------------------------------------------------------------


{-| The acting user. On the public site this is a single local pseudo-user; the bbx app injects the
logged-in user instead (the rest of the code is identical). -}
ctx : Context
ctx =
    { user = "me", groups = [] }


{-| Notebooks are kept in the browser's persistent storage, under the `elm-notebook` namespace. -}
backend : Backend (Workspace.Msg NbMsg)
backend =
    Workspace.Browser.backend "elm-notebook"



-- MODEL ----------------------------------------------------------------------


type alias Model =
    { ws : Workspace.Model NbDoc }


type Msg
    = WsMsg (Workspace.Msg NbMsg)


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( ws, cmd ) =
            Workspace.init backend
    in
    ( { ws = ws }, Cmd.map WsMsg cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update (WsMsg m) model =
    let
        ( ws, cmd ) =
            Workspace.update NB.config backend ctx m model.ws
    in
    ( { ws = ws }, Cmd.map WsMsg cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map WsMsg (Workspace.subscriptions model.ws)



-- VIEW -----------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div [ HA.class "nb-app" ]
        [ pageHeader
        , Html.map WsMsg (Workspace.view NB.config backend ctx model.ws)
        , pageFooter
        ]


pageHeader : Html Msg
pageHeader =
    header [ HA.class "nb-hero" ]
        [ div [ HA.class "nb-hero-inner" ]
            [ span [ HA.class "nb-eyebrow" ] [ text "elm · data exploration" ]
            , h1 [] [ text "elm-notebook" ]
            , p [ HA.class "nb-lead" ]
                [ text "A workspace of Jupyter-style notebooks that run real "
                , a [ HA.href "https://elm-lang.org" ] [ text "Elm" ]
                , text " in your browser. Create a notebook, share it, comment on cells, import data "
                , text "and export any step — the app suggests where to go next as you explore."
                ]
            ]
        ]


pageFooter : Html Msg
pageFooter =
    footer [ HA.class "nb-foot" ]
        [ div []
            [ text "elm-notebook — runs real Elm via the "
            , a [ HA.href "https://github.com/tunguski/elm-lang" ] [ text "elm-lang" ]
            , text " interpreter, in a reusable "
            , a [ HA.href "https://github.com/tunguski/elm-workspace" ] [ text "elm-workspace" ]
            , text "."
            ]
        , div [ HA.class "nb-foot-links" ]
            [ a [ HA.href "tests.html" ] [ text "Test report" ]
            , a [ HA.href "https://github.com/tunguski/elm-notebook" ] [ text "GitHub" ]
            , a [ HA.href "https://tunguski.github.io/" ] [ text "More projects" ]
            ]
        ]
