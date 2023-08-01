#
# This module manages the generic pretty printing.
#
# Takes a TextTree and turns it into a Text, breaking rows when they get too "complex".
#

[# Tests to add:


makeTree as fn [TextTree]: Text =
    fn items:
    { open = "[", separator = ",", close = "]", items }
    >> list
    >> display "...." __



* []
* [ text "a", text "b" ]
* [ text "a", text "bbbbbbbbbbbbbbbbbbbbbbbbbb" ]
* [
    , text "a"
    , list { open = "{", separator = ",", close = "}", items = [
        , text "Z"
        , text "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      ] }
    ]


#]


# This is just an arbitrary estimate of the complexity of a line
# Right now, we just use string length
alias Complexity =
    Int


union IsIndented =

    [#
        a
        b
        c
    #]
    , NotIndented

    [#
        header
            a
            b
            c
    #]
    , IndentedWithHeader Text


union TextTree =
  #
  # Multiple spans can be joined in a single horizontal row, but only until Complexity reaches a certain threshold
  #
  , Span Complexity Text

  #
  # All lines in a block occupy their own rows and will be joined with the rest only vertically
  #
  , Block IsIndented [TextTree]



#
# Basic combinators
#


text as fn Text: TextTree =
    fn text:

    Span (Text.length text) text


stack as fn [TextTree]: TextTree =
    fn content:

    Block NotIndented content


#
# a b c
#
# or
#
# a
#    b
#    c
#
rowOrIndented as fn Text, [TextTree]: TextTree =
    fn header, content:

    try maybeSpan 0 [] [text (header .. " "), ...content] as
        , Just span:
            span

        , Nothing:
            Block (IndentedWithHeader header) content


#
# previous a b c
#
# or
#
# previous a
#    b
#    c
#
rowOrHead as fn Text, [TextTree]: TextTree =
    #TODO Needs a change in the TextTree type
    rowOrIndented


separatedBy as fn Text, [TextTree]: TextTree =
    fn separator, content:

    try maybeSpan 0 [] (List.intersperse (text separator) content []) as
        , Just span:
            span

        , Nothing:
            content
            >> List.map (fn c: rowOrIndented separator [c]) __
            >> Block NotIndented __


# TODO The fact that I need this function is a code smell
addSeparator as fn Text, TextTree: TextTree =
    fn separator, tree:

    try tree as
        , Span complexity text:
            Span complexity (separator .. " " .. text)

        , Block (IndentedWithHeader header) rows:
            Block (IndentedWithHeader (separator .. " " .. header)) rows

        , Block NotIndented [head, ...tail]:
            Block NotIndented [addSeparator separator head, ...tail]

        , Block NotIndented []:
            tree


list as fn { open as Text, separator as Text, close as Text, items as [TextTree]}: TextTree =
    fn ({ open, separator, close, items }):

    zero = Text.length open + Text.length close

    if items == [] then
        text (open .. close)
    else
        try maybeSpan zero [open .. " "] (List.intersperse (text (separator .. " ")) items []) as
            , Just (Span com t):
                Span com (t .. " " .. close)

            , _:
                [
                , List.map (addSeparator separator __) items
                , [text close]
                ]
                >> List.concat
                >> Block (IndentedWithHeader open) __


maybeSpan as fn Int, [Text], [TextTree]: Maybe TextTree =
    fn complexityAcc, textAcc, content:

    if complexityAcc > 20 then
      Nothing
    else
      try content as
          , []:
              textAcc
              >> List.reverse
              >> Text.join "" __
              >> Span complexityAcc __
              >> Just

          , Span complexity snippet :: tail:
              maybeSpan (complexity + complexityAcc) (snippet :: textAcc) tail

          , _:
              Nothing



#
# The 1st parameter is the indent accumulator. Use "".
#
toText as fn Text, TextTree: Text =
    fn indent, tree:

    try tree as
        , Span _ content:
            indent .. content

        , Block NotIndented content:
            content
            >> List.map (toText indent __) __
            >> Text.join "\n" __

        , Block (IndentedWithHeader header) content:

            head =
                indent .. header

            tail =
                content
                >> List.map (toText (indent .. "    ") __) __

            head :: tail >> Text.join "\n" __
