module Main exposing (main)

{-| The elm-notebook site.

Two views, switched by a top nav and reflected in the URL hash so a page is safe to reload and
links are shareable:

  - **Examples** (`#`) — a standalone notebook playground (the original single-notebook experience:
    guided lessons, a live notebook, suggestions), with nothing saved.
  - **Workspace** (`#workspace`, and `#doc/<uuid>` per document) — many saved notebooks with naming,
    search, copy, sharing/permissions, comments, import, SQL and export, from the reusable
    [`Workspace`](Workspace) component over the [`Notebook.Workspace`](Notebook-Workspace) document.

-}

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, footer, h1, header, nav, p, span, text)
import Html.Attributes as HA
import Html.Events as HE
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
    }


type Msg
    = WsMsg (Workspace.Msg NbMsg)
    | DemoMsg NbMsg
    | SetRoute Route
    | GotHash String


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( ws, wsCmd ) =
            Workspace.init backend
    in
    ( { route = Examples, ws = ws, demo = NB.examples }
    , Cmd.batch [ Cmd.map WsMsg wsCmd, Nav.getHash GotHash ]
    )



-- UPDATE ---------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotHash raw ->
            -- routing comes *from* the URL here, so don't write the hash back
            applyHash raw model

        _ ->
            let
                ( next, cmd ) =
                    updateInner msg model
            in
            ( next, Cmd.batch [ cmd, syncHash model next ] )


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

        GotHash _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map WsMsg (Workspace.subscriptions model.ws)



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


syncHash : Model -> Model -> Cmd Msg
syncHash old next =
    if toHash old == toHash next then
        Cmd.none

    else
        Nav.setHash (toHash next)


{-| Apply a hash read from the URL: pick the route and, for `doc/<id>`, ask the workspace to open
that document. -}
applyHash : String -> Model -> ( Model, Cmd Msg )
applyHash raw model =
    let
        h =
            normalizeHash raw
    in
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
    raw
        |> dropPrefixChar '#'
        |> dropPrefixChar '/'


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
        [ topNav model.route
        , case model.route of
            Examples ->
                div []
                    [ pageHeader
                    , Html.map DemoMsg (NB.examplesView model.demo)
                    ]

            Wsp ->
                Html.map WsMsg (Workspace.view NB.config backend ctx model.ws)
        , pageFooter
        ]


topNav : Route -> Html Msg
topNav route =
    nav [ HA.class "nb-topnav" ]
        [ span [ HA.class "nb-brand" ] [ text "elm-notebook" ]
        , div [ HA.class "nb-topnav-tabs" ]
            [ tab "Examples" (route == Examples) (SetRoute Examples)
            , tab "Workspace" (route == Wsp) (SetRoute Wsp)
            ]
        ]


tab : String -> Bool -> Msg -> Html Msg
tab label active msg =
    button
        [ HA.class
            ("nb-tab"
                ++ (if active then
                        " nb-tab-active"

                    else
                        ""
                   )
            )
        , HE.onClick msg
        ]
        [ text label ]


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
