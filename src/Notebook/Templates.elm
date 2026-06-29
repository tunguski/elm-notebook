module Notebook.Templates exposing (Template, all, byId)

{-| **Starter templates** for a new notebook. Unlike the guided [lessons](Notebook-Suggest) (which
teach the language step by step), a template is a small *working analysis* — a dataset already loaded
and a first few results computed — meant to be opened and adapted. The "New from template" menu lists
[`all`](#all); the host builds a document from the chosen template's cells.

@docs Template, all, byId

-}

import Notebook.Cell exposing (CellKind(..))


{-| A named, described scaffold: the cells a new notebook starts with. -}
type alias Template =
    { id : String, title : String, blurb : String, cells : List ( CellKind, String ) }


{-| Look a template up by id. -}
byId : String -> Maybe Template
byId wanted =
    List.filter (\t -> t.id == wanted) all |> List.head


{-| Every starter template, in menu order. -}
all : List Template
all =
    [ blank, sales, plotting, stats, formatting ]


blank : Template
blank =
    Template "blank"
        "Blank notebook"
        "An empty page with one note and one code cell."
        [ ( Markdown, "# Untitled\n\nDescribe what you're exploring, then add a code cell." )
        , ( Code, "" )
        ]


sales : Template
sales =
    Template "sales"
        "Sales analysis"
        "A sales table, filtered, grouped by region and summed."
        [ ( Markdown, "# Sales analysis\n\nA small orders table. Filter it, then **group by region** and total the units." )
        , ( Code, "sales =\n    [ { product = \"Pen\", region = \"North\", units = 120 }\n    , { product = \"Pen\", region = \"South\", units = 80 }\n    , { product = \"Mug\", region = \"North\", units = 45 }\n    , { product = \"Mug\", region = \"South\", units = 70 }\n    , { product = \"Notebook\", region = \"North\", units = 200 }\n    ]" )
        , ( Markdown, "## Totals by region\n\n`summarize` groups the rows by a key, then sums and averages a chosen field." )
        , ( Code, "summarize (\\r -> r.region) (\\r -> toFloat r.units) sales" )
        , ( Code, "-- the strong orders only\nList.filter (\\r -> r.units > 75) sales" )
        ]


plotting : Template
plotting =
    Template "plotting"
        "Function plot"
        "Sample a function over a range and chart it."
        [ ( Markdown, "# Function plot\n\n`plotPoints f lo hi` samples a function into `{x, y}` rows. Toggle the **Line** chart on the result below." )
        , ( Code, "plotPoints (\\x -> x * x) (-5) 5" )
        , ( Markdown, "## Compare two curves\n\n`linspace lo hi n` gives evenly spaced inputs; map a record over them for a multi-series table." )
        , ( Code, "xs = linspace (-5) 5 41" )
        , ( Code, "List.map (\\x -> { x = x, square = x * x, cube = x * x * x }) xs" )
        ]


stats : Template
stats =
    Template "stats"
        "Summary statistics"
        "Describe a sample, then look at its spread."
        [ ( Markdown, "# Summary statistics\n\nA sample of measurements. `describe` reports count, mean, min, max, median and standard deviation at once.\n\nThe variance is $\\sigma^2 = \\frac{1}{n}\\sum (x_i - \\mu)^2$ and the mean is $\\mu$." )
        , ( Code, "sample = [ 12.1, 9.8, 11.2, 14.6, 10.0, 13.3, 8.9, 12.7, 11.5, 10.4 ]" )
        , ( Code, "describe sample" )
        , ( Code, "-- the middle of the distribution\nmedian sample" )
        , ( Code, "-- how spread out it is\nstdev sample" )
        ]


formatting : Template
formatting =
    Template "formatting"
        "Formatting guide"
        "Callouts, links, images and inline math in Markdown cells."
        [ ( Markdown, "# Formatting guide\n\nText cells render a small Markdown dialect — **bold**, `code`, headings and lists — plus a few extras shown here." )
        , ( Markdown, "## Callouts\n\nStart a quote line with a `[!kind]` marker:\n\n> [!note] These boxes draw attention.\n\n> [!tip] Use them for asides and hints.\n\n> [!warning] And for things to watch out for." )
        , ( Markdown, "## Links & images\n\nLink like [the elm-lang repo](https://github.com/tunguski/elm-lang), and embed an image with `![alt](url)`:\n\n![a badge](data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='150' height='30'><rect width='150' height='30' rx='6' fill='%235b6ef5'/><text x='75' y='20' fill='white' font-family='sans-serif' font-size='14' text-anchor='middle'>elm-notebook</text></svg>)" )
        , ( Markdown, "## Math\n\nInline math sits between dollar signs: the area of a circle is $A = \\pi r^2$, and the mean is $\\mu = \\frac{1}{n}\\sum x_i$." )
        , ( Markdown, "## Tables & checklists\n\nPipe tables render as a grid:\n\n| Fruit | Colour | Qty |\n| --- | --- | --- |\n| Apple | red | 12 |\n| Lime | green | 7 |\n\nAnd task lists become checkboxes:\n\n- [x] write the notebook\n- [ ] chart the results\n- [ ] share the link" )
        , ( Markdown, "## Code, rules & emphasis\n\nYou can ~~strike out~~ text or ==highlight== it.\n\n---\n\nFenced blocks show highlighted Elm:\n\n```elm\nmean xs =\n    List.sum xs / toFloat (List.length xs)\n```" )
        ]
