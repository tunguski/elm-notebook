module Main exposing (main)

{-| The elm-notebook site — a [`Workspace.Site`](Workspace-Site).

The landing (`#`) is the standalone notebook playground (the original single-notebook experience:
guided lessons, a live notebook, suggestions), with a "Copy to workspace" button that lifts the
current playground into a saved workspace document. The workspace (`#workspace`, `#<uuid>`) manages
many saved notebooks — naming, search, copy, sharing, comments, import, SQL and export — over the
[`Notebook.Workspace`](Notebook-Workspace) document.

All the routing, navbar, hero and footer chrome lives in [`Workspace.Site`](Workspace-Site); this
module only declares what is specific to elm-notebook.

-}

import Html exposing (text)
import Html.Attributes as HA
import Notebook.Workspace as NB exposing (NbDoc, NbMsg)
import Workspace.Site


main : Program () (Workspace.Site.Model NbDoc NbDoc) (Workspace.Site.Msg NbMsg NbMsg)
main =
    Workspace.Site.program
        { title = "elm-notebook"
        , namespace = "elm-notebook"
        , logo = "logo.svg"
        , eyebrow = "elm · data exploration"
        , lead =
            [ text "A Jupyter-style notebook that runs real "
            , Html.a [ HA.href "https://elm-lang.org" ] [ text "Elm" ]
            , text " in your browser — edit a cell, press Run, and build an analysis step by step, with "
            , text "suggestions for where to go next. Open the "
            , Workspace.Site.workspaceLink [ text "Workspace" ]
            , text " to save, share and organise many notebooks."
            ]
        , repoUrl = "https://github.com/tunguski/elm-notebook"
        , workspace = NB.config
        , context = { user = "me", groups = [] }
        , landing =
            { init = NB.examples
            , update = \msg doc -> ( NB.updateNb msg doc, Cmd.none )
            , subscriptions = \_ -> Sub.none
            , view = NB.examplesView
            , copyToWorkspace =
                \msg doc ->
                    if NB.isCopyToWorkspace msg then
                        Just doc

                    else
                        Nothing
            }
        }
