module Main exposing (main)

{-| The elm-notebook site.

Two views, reflected in the URL hash so a page is safe to reload, links are shareable, and the
browser Back/Forward buttons work:

  - **Examples** (`#`) — a standalone notebook playground (the original single-notebook experience:
    guided lessons, a live notebook, suggestions), with nothing saved. The hero links into the
    workspace.
  - **Workspace** (`#workspace`, and `#<uuid>` per document) — many saved notebooks with naming,
    search, copy, sharing/permissions, comments, import, SQL and export, from the reusable
    [`Workspace`](Workspace) component over the [`Notebook.Workspace`](Notebook-Workspace) document.

There is no top navbar: the workspace is reached from the hero link, and the browser's Back button
returns to the examples (the app polls the URL hash, since `Browser.element` has no hash-change
subscription and `Browser.application` would intercept the data-URI export download links).

-}

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, footer, h1, header, p, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Notebook.Workspace as NB exposing (NbDoc, NbMsg)
import Time
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


ctx : Context
ctx =
    { user = "me", groups = [] }


backend : Backend (Workspace.Msg NbMsg)
backend =
    Workspace.Browser.backend "elm-notebook"



-- MODEL ----------------------------------------------------------------------


type Route
    = Examples
    | Wsp


type alias Model =
    { route : Route
    , ws : Workspace.Model NbDoc
    , demo : NbDoc
    , hash : String
    }


type Msg
    = WsMsg (Workspace.Msg NbMsg)
    | DemoMsg NbMsg
    | SetRoute Route
    | GotHash String
    | Poll


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( ws, wsCmd ) =
            Workspace.init backend
    in
    ( { route = Examples, ws = ws, demo = NB.examples, hash = "" }
    , Cmd.batch [ Cmd.map WsMsg wsCmd, Nav.getHash GotHash ]
    )



-- UPDATE ---------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotHash raw ->
            -- the URL changed (initial load, or Back/Forward) — route from it, don't write it back
            let
                h =
                    normalizeHash raw
            in
            if h == model.hash then
                ( model, Cmd.none )

            else
                applyHash h { model | hash = h }

        Poll ->
            ( model, Nav.getHash GotHash )

        _ ->
            let
                ( next, cmd ) =
                    updateInner msg model

                desired =
                    toHash next
            in
            if desired == next.hash then
                ( next, cmd )

            else
                ( { next | hash = desired }, Cmd.batch [ cmd, Nav.setHash desired ] )


updateInner : Msg -> Model -> ( Model, Cmd Msg )
updateInner msg model =
    case msg of
        WsMsg m ->
            let
                ( ws, cmd ) =
                    Workspace.update NB.config backend ctx m model.ws
            in
            ( { model | ws = ws }, Cmd.map WsMsg cmd )

        DemoMsg m ->
            ( { model | demo = NB.updateNb m model.demo }, Cmd.none )

        SetRoute route ->
            ( { model | route = route }, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map WsMsg (Workspace.subscriptions model.ws)

        -- poll the URL hash so the browser Back/Forward buttons change the view
        , Time.every 400 (always Poll)
        ]



-- ROUTING --------------------------------------------------------------------


{-| The hash this model should show. -}
toHash : Model -> String
toHash model =
    case model.route of
        Examples ->
            ""

        Wsp ->
            case model.ws.open of
                Just stored ->
                    stored.meta.id

                Nothing ->
                    "workspace"


{-| Route from a hash read off the URL (already normalised); model.hash is assumed up to date. -}
applyHash : String -> Model -> ( Model, Cmd Msg )
applyHash h model =
    if h == "" then
        ( { model | route = Examples }, Cmd.none )

    else if h == "workspace" then
        ( { model | route = Wsp }, Cmd.none )

    else
        -- any other hash is a document id (a uuid)
        ( { model | route = Wsp }
        , Cmd.map WsMsg (Workspace.openDocument backend h)
        )


normalizeHash : String -> String
normalizeHash raw =
    raw |> dropPrefixChar '#' |> dropPrefixChar '/'


dropPrefixChar : Char -> String -> String
dropPrefixChar c s =
    if String.startsWith (String.fromChar c) s then
        String.dropLeft 1 s

    else
        s



-- VIEW -----------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div [ HA.class "nb-app" ]
        [ case model.route of
            Examples ->
                div []
                    [ pageHeader
                    , Html.map DemoMsg (NB.examplesView model.demo)
                    ]

            Wsp ->
                Html.map WsMsg (Workspace.view NB.config backend ctx model.ws)
        , pageFooter
        ]


pageHeader : Html Msg
pageHeader =
    header [ HA.class "nb-hero" ]
        [ div [ HA.class "nb-hero-inner" ]
            [ span [ HA.class "nb-eyebrow" ] [ text "elm · data exploration" ]
            , h1 [] [ text "elm-notebook" ]
            , p [ HA.class "nb-lead" ]
                [ text "A Jupyter-style notebook that runs real "
                , a [ HA.href "https://elm-lang.org" ] [ text "Elm" ]
                , text " in your browser. Edit a cell, press Run, and build an analysis step by step — "
                , text "the app suggests where to go next. Open the "
                , button [ HA.class "nb-inline-link", HE.onClick (SetRoute Wsp) ] [ text "Workspace" ]
                , text " to save, share and organise many notebooks."
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
