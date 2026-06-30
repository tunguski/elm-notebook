module Notebook.I18n exposing (T, en)

{-| **User-interface translations for the notebook editor.**

A single typed record of every visible chrome string in the notebook view (toolbar, command
palette, side panels, cell controls and dialogs). The host supplies one [`T`](#T) value via
[`Notebook.Workspace.Options`](Notebook-Workspace#Options); the standalone site uses [`en`](#en),
and an embedder (e.g. BlueBox) passes its own translation — Polish, say — built from this same type.

Why a record of fields rather than a `key -> String` lookup: a missing or misspelt string becomes a
**compile error**, not a silent runtime blank, and parameterised strings stay type-safe (they are
just functions in the record). Things that are *not* language are intentionally left inline in the
views and absent here: keyboard keys (`"Enter"`, `"ArrowUp"`), math/markdown glyphs, the `▶` run
arrows, value renderings (`"True"`), and the Jupyter `In`/`Out` prompt labels. Content that is data
rather than chrome — guided-lesson and template text, the function reference — is provided
separately per host.

@docs T, en

-}


{-| Every visible chrome string in the notebook editor. -}
type alias T =
    { -- toolbar
      runAll : String
    , rerunAffected : String
    , addCode : String
    , addText : String
    , addInput : String
    , importData : String
    , find : String
    , reference : String
    , actions : String
    , templates : String
    , slides : String
    , share : String
    , clearOutputs : String
    , copyToWorkspace : String
    , toggleDark : String
    , undo : String
    , redo : String
    , reportMode : String
    , editMode : String

    -- command palette
    , palettePlaceholder : String
    , paRunAll : String
    , paAddCode : String
    , paAddText : String
    , paAddInput : String
    , paClearOutputs : String
    , paFindReplace : String
    , paFunctionReference : String
    , paImportData : String
    , paNewFromTemplate : String
    , paSlideshow : String
    , paShareLink : String
    , paToggleReport : String
    , paUndo : String
    , paRedo : String

    -- reference panel
    , searchFunctions : String
    , close : String

    -- find & replace
    , findInCells : String
    , replaceWith : String
    , replaceAll : String

    -- import dialog
    , pasteData : String
    , namePlaceholder : String
    , dataPlaceholder : String
    , importToCell : String
    , cancel : String

    -- slides
    , prev : String
    , next : String
    , edit : String
    , slidesHint : String

    -- share dialog
    , shareThisNotebook : String
    , shareCopyHint : String
    , sharePasteHint : String
    , shareTokenPlaceholder : String
    , loadShared : String

    -- templates / lessons bars
    , newFromTemplate : String
    , guidedLessons : String

    -- cell action toolbar
    , expand : String
    , staleHint : String
    , run : String
    , runCellsAbove : String
    , runFromHereDown : String
    , insertCellAbove : String
    , insertCellBelow : String
    , duplicate : String
    , collapse : String
    , collapseSection : String
    , expandSection : String
    , toCode : String
    , toText : String
    , hasComments : String
    , replaceAndRerun : String

    -- table / chart controls
    , group : String
    , groupBy : String
    , pivot : String
    , profile : String
    , corr : String
    , heat : String
    , bars : String
    , summary : String
    , table : String
    , columnsControl : String
    , rows : String
    , value : String
    , hideColumn : String
    , showColumn : String
    , showAll : String
    , showFewer : String
    , addFilter : String
    , needsNumericTable : String
    , sortByColumn : String
    , filterRows : String
    , columns : String
    , filterSuffix : String
    , columnPlaceholder : String
    , valuePlaceholder : String
    , removeFilter : String

    -- input control kinds
    , slider : String
    , checkbox : String
    , number : String

    -- suggestions panel
    , suggestedNextSteps : String
    , suggestedLead : String

    -- overview panel
    , overviewTitle : String
    , ovCells : String
    , ovCode : String
    , ovText : String
    , ovVariables : String
    , ovErrors : String
    , ovWords : String
    , ovRead : String
    , minutesSuffix : String

    -- outline / variables / errors panels
    , outlineTitle : String
    , variablesTitle : String
    , variablesEmpty : String
    , insertCellFor : String -> String
    , errorsCount : Int -> String
    , rerunThisCell : String
    }


{-| English — the default used by the standalone site. -}
en : T
en =
    { runAll = "▶▶ Run all"
    , rerunAffected = "Re-run the cells affected by your edits"
    , addCode = "+ Code cell"
    , addText = "+ Text cell"
    , addInput = "+ Input"
    , importData = " Import data"
    , find = " Find"
    , reference = " Reference"
    , actions = " Actions"
    , templates = " Templates"
    , slides = " Slides"
    , share = " Share"
    , clearOutputs = "Clear outputs"
    , copyToWorkspace = " Copy to workspace"
    , toggleDark = "Toggle dark mode"
    , undo = "Undo"
    , redo = "Redo"
    , reportMode = " Report"
    , editMode = " Edit"
    , palettePlaceholder = "Type an action…"
    , paRunAll = "Run all"
    , paAddCode = "Add code cell"
    , paAddText = "Add text cell"
    , paAddInput = "Add input"
    , paClearOutputs = "Clear outputs"
    , paFindReplace = "Find & replace"
    , paFunctionReference = "Function reference"
    , paImportData = "Import data"
    , paNewFromTemplate = "New from template"
    , paSlideshow = "Slideshow"
    , paShareLink = "Share link"
    , paToggleReport = "Toggle report mode"
    , paUndo = "Undo"
    , paRedo = "Redo"
    , searchFunctions = "Search functions…"
    , close = "Close"
    , findInCells = "Find in cells…"
    , replaceWith = "Replace with…"
    , replaceAll = "Replace all"
    , pasteData = "Paste data — a JSON array of objects, or CSV / TSV"
    , namePlaceholder = "name"
    , dataPlaceholder = "[ { \"city\": \"Oslo\", \"pop\": 700000 }, … ]"
    , importToCell = "Import → cell"
    , cancel = "Cancel"
    , prev = " Prev"
    , next = "Next "
    , edit = " Edit"
    , slidesHint = "Add a “# heading” to a text cell to start a slide."
    , shareThisNotebook = " Share this notebook"
    , shareCopyHint = "Copy this link — it carries the whole notebook:"
    , sharePasteHint = "…or paste a shared link / token to load it:"
    , shareTokenPlaceholder = "#nb=… or token"
    , loadShared = "Load shared notebook"
    , newFromTemplate = " New from template"
    , guidedLessons = "Guided lessons:"
    , expand = "Expand"
    , staleHint = "Stale — an upstream cell changed; Run to refresh"
    , run = "▶ Run"
    , runCellsAbove = "Run the cells above"
    , runFromHereDown = "Run from here down"
    , insertCellAbove = "Insert cell above"
    , insertCellBelow = "Insert cell below"
    , duplicate = "Duplicate"
    , collapse = "Collapse"
    , collapseSection = "Collapse section"
    , expandSection = "Expand section"
    , toCode = "To code"
    , toText = "To text"
    , hasComments = "This cell has comments"
    , replaceAndRerun = "Replace it and re-run"
    , group = "Group"
    , groupBy = "Group by"
    , pivot = "Pivot"
    , profile = "Profile"
    , corr = "Corr"
    , heat = "Heat"
    , bars = "Bars"
    , summary = "Σ Summary"
    , table = "Table"
    , columnsControl = "Columns"
    , rows = "Rows"
    , value = "Value"
    , hideColumn = "Hide this column"
    , showColumn = "Show this column"
    , showAll = "Show all "
    , showFewer = "Show fewer"
    , addFilter = "+ filter"
    , needsNumericTable = "Needs a table with numeric columns."
    , sortByColumn = "Sort by this column"
    , filterRows = "Filter rows…"
    , columns = "columns:"
    , filterSuffix = " filter"
    , columnPlaceholder = "column…"
    , valuePlaceholder = "value"
    , removeFilter = "Remove filter"
    , slider = "Slider"
    , checkbox = "Checkbox"
    , number = "Number"
    , suggestedNextSteps = "Suggested next steps"
    , suggestedLead = "Based on your last result. Click one to add it as a new cell."
    , overviewTitle = "Notebook"
    , ovCells = "Cells"
    , ovCode = "Code"
    , ovText = "Text"
    , ovVariables = "Variables"
    , ovErrors = "Errors"
    , ovWords = "Words"
    , ovRead = "Read"
    , minutesSuffix = "min"
    , outlineTitle = "Outline"
    , variablesTitle = "Variables"
    , variablesEmpty = "Names you define with = appear here."
    , insertCellFor = \name -> "Insert a cell for " ++ name
    , errorsCount = \n -> " " ++ String.fromInt n ++ " in error"
    , rerunThisCell = "Re-run this cell"
    }
