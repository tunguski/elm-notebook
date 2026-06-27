module Notebook.Cell exposing
    ( Cell, CellKind(..), Output(..)
    , Control(..), InputSpec
    , markdown, code, inputCell
    , inputSource, setInputValue, setInputName, setInputControl, controlValueDefault
    , isCode, isMarkdown, isExecutable, hasError
    )

{-| A single notebook cell — the unit a notebook is made of.

A cell is **Markdown** (prose), **Code** (one expression the kernel evaluates), or an **Input**
widget — a slider / number / text / checkbox that binds a name to a value via a UI control. An
input cell is really just a `name = literal` binding whose source is kept in sync with the
control, so the kernel runs it like any other code cell (this is how a notebook gets interactive
parameters). A code/input cell remembers its last [`Output`](#Output) and execution count.

@docs Cell, CellKind, Output
@docs Control, InputSpec
@docs markdown, code, inputCell
@docs inputSource, setInputValue, setInputName
@docs isCode, isMarkdown, isExecutable, hasError

-}

import Lang exposing (Value)


{-| The three kinds of cell. -}
type CellKind
    = Markdown
    | Code
    | Input


{-| The control an input cell exposes. `Slider min max step`. -}
type Control
    = Slider Float Float Float
    | NumberBox
    | TextBox
    | Checkbox


{-| An input widget: the name it binds, its control, and the current value (as the literal text
that will appear on the right of the `=`).
-}
type alias InputSpec =
    { name : String
    , control : Control
    , value : String
    }


{-| The result of running a code/input cell: nothing yet, a value, or an error message. -}
type Output
    = OutNone
    | OutValue Value
    | OutError String


{-| A cell. `input` is present only for `Input` cells. -}
type alias Cell =
    { id : Int
    , kind : CellKind
    , source : String
    , output : Output
    , count : Maybe Int
    , input : Maybe InputSpec
    }


{-| A fresh markdown cell. -}
markdown : Int -> String -> Cell
markdown id source =
    { id = id, kind = Markdown, source = source, output = OutNone, count = Nothing, input = Nothing }


{-| A fresh, un-run code cell. -}
code : Int -> String -> Cell
code id source =
    { id = id, kind = Code, source = source, output = OutNone, count = Nothing, input = Nothing }


{-| A fresh input-widget cell; its source is the `name = literal` binding it produces. -}
inputCell : Int -> InputSpec -> Cell
inputCell id spec =
    { id = id, kind = Input, source = inputSource spec, output = OutNone, count = Nothing, input = Just spec }


{-| The Elm binding an input spec produces: `name = <literal>` (text values are quoted, numbers
and booleans are bare).
-}
inputSource : InputSpec -> String
inputSource spec =
    spec.name ++ " = " ++ literal spec


literal : InputSpec -> String
literal spec =
    case spec.control of
        TextBox ->
            "\"" ++ spec.value ++ "\""

        _ ->
            spec.value


{-| Set an input cell's value, keeping its source binding in sync (done here, inside the module
that owns `InputSpec`, to avoid a cross-module record-update miscompile). A no-op on other cells.
-}
setInputValue : String -> Cell -> Cell
setInputValue value cell =
    case cell.input of
        Just spec ->
            let
                updated =
                    { spec | value = value }
            in
            { cell | input = Just updated, source = inputSource updated, output = OutNone, count = Nothing }

        Nothing ->
            cell


{-| Rename an input cell's bound name, keeping its source in sync. -}
setInputName : String -> Cell -> Cell
setInputName name cell =
    case cell.input of
        Just spec ->
            let
                updated =
                    { spec | name = name }
            in
            { cell | input = Just updated, source = inputSource updated, output = OutNone, count = Nothing }

        Nothing ->
            cell


{-| Change an input cell's control type, resetting its value to that control's default. -}
setInputControl : Control -> Cell -> Cell
setInputControl control cell =
    case cell.input of
        Just spec ->
            let
                updated =
                    { spec | control = control, value = controlValueDefault control }
            in
            { cell | input = Just updated, source = inputSource updated, output = OutNone, count = Nothing }

        Nothing ->
            cell


{-| A sensible starting value for a control. -}
controlValueDefault : Control -> String
controlValueDefault control =
    case control of
        Slider mn _ _ ->
            String.fromFloat mn

        NumberBox ->
            "0"

        TextBox ->
            "hello"

        Checkbox ->
            "True"


{-| Is this a code cell? -}
isCode : Cell -> Bool
isCode cell =
    cell.kind == Code


{-| Is this a markdown cell? -}
isMarkdown : Cell -> Bool
isMarkdown cell =
    cell.kind == Markdown


{-| Does this cell run in the kernel (code or input)? -}
isExecutable : Cell -> Bool
isExecutable cell =
    cell.kind == Code || cell.kind == Input


{-| Did the cell's last run end in an error? -}
hasError : Cell -> Bool
hasError cell =
    case cell.output of
        OutError _ ->
            True

        _ ->
            False
