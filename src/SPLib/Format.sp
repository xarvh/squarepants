#
# This module is a verbatim translation of https://codeberg.org/avh4/prettyprint-avh4/src/branch/main/src/Text/PrettyPrint/Avh4/
# (Both Block.hs and Indent.hs have been merged here)
#

# I'm too tired to use NonEmpty
for1 as fn [ a ], fn a, a: a: a =
    fn items, f:
    try items as
        []: todo "for1 got an empty list...."
        [ head, tail... ]: List.for head tail f


#
# Indent.hs
#

spacesInTab as Int =
    4


# | `Indent` represents an indentation level,
# and the operator `<>` can be used to combine two indentations side-by-side, accounting for the tab size.
#
# Each `Indent` can be thought of as:
# one or more TABs, followed by zero to three SPACEs.
#
# Combining two indents can be thought of as
# typing the first and then the second sequence of
# TABs and SPACEs in a word processor.
#
# For example:
#
#    [TAB] <> [TAB]     ==  [TAB][TAB]
#    [TAB] <> ...       ==  [TAB]...
#    [TAB] <> [TAB]...  ==  [TAB][TAB]...
#     <> ...            ==  ...
#    [TAB].. <> [TAB]   ==  [TAB][TAB]
#    .. <> .            ==  ...
#    .. <> ..           ==  [TAB]
Indent =
    [ Int ]


#instance Eq Indent where
#  a == b =
#    width' a == width' b

indent_tab as Indent =
    [ spacesInTab ]


indent_spaces as fn Int: Indent =
    fn n:
    [ n ]


indent_width as fn Indent: Int =
    List.for 0 __ (__ + __)


# TODO: Squarepants does not have a strategy to deal with Tabs vs Spaces, it just rejects them.
# This needs to be cleared up with a proper strategy to deal with everything.
# Maybe just allowing a command to transform a module from spaces to tabs and vice versa

[#
if i < spacesInTab then
  # The right side starts with spaces (and no TABs),
  # so just add everything together.
else
  # The right side starts with at least one TAB,
  # so remove the trailing spaces from the left.
  pos - (modBy spacesInTab pos) + i
#]

#
# Block.hs
#

# | A `Line` is ALWAYS just one single line of text,
# and can always be combined horizontally with other `Line`s.
#
# - `Space` is a single horizontal space,
# - `Blank` is a line with no content.
# - `Text` brings any text into the data structure. (Uses `ByteString.Builder` for the possibility of optimal performance)
# - `Row` joins multiple elements onto one line.
var Line =
    , 'text_ Text
    , 'commentWithIndent Text
    , 'commentIgnoreIndent Text
    , 'row Line Line
    , 'space
    , 'blank


# | Creates a @Line@ from the given @Char@.
# You must guarantee that the given character is a valid 7-bit ASCII character,
# and is not a space character (use `space` instead).
#char7 as fn Text: Line =
#    Text_

# | Creates a @Line@ from the given @String@.
# You must guarantee that all characters in the @String@ are valid 7-bit ASCII characters,
# and that the string does not start or end with spaces (use `space` instead).
#string7 as fn Text: Line =
#    Text_

# | If you know the String only contains ASCII characters, then use `string7` instead for better performance.
#stringUtf8 as fn Text: Line =
#    Text_

# | A @Line@ containing a single space.  You **must** use this to create
# space characters if the spaces will ever be at the start or end of a line that is joined in the context of indentation changes.
space as Line =
    'space


var Indented a =
    , 'indented Indent a


indent_map as fn fn Line: Line, Indented Line: Indented Line =
    fn f, 'indented indent l:
    'indented indent (f l)


# | `Block` contains Lines (at least one; it can't be empty).
#
# Block either:
#
#  - can appear in the middle of a line
#      (Stack someLine [], thus can be joined without problems), or
#  - has to appear on its own
#      (Stack someLine moreLines OR MustBreak someLine).
#
# Types of Blocks:
#
# - `SingleLine` is a single line, and the indentation level for the line.
# - `MustBreak` is a single line (and its indentation level)) that cannot have anything joined to its right side.
#  Notably, it is used for `--` comments.
# - `Stack` contains two or more lines, and the indentation level for each.
#
# Sometimes (see `prefix`) the first line of Stack
#  gets different treatment than the other lines.
var Block =
    , 'singleLine RequiredLineBreaks (Indented Line)
    , 'stack (Indented Line) [ Indented Line ]
    , 'empty


var RequiredLineBreaks =
    , 'mustBreakAtEnd
    , 'noRequiredBreaks


blockAsLine as fn Block: Maybe Line =
    fn b:
    try b as
        'singleLine _ ('indented _ l): 'just l
        _: 'nothing


# | A blank line (taking up one vertical space), with no text content.
blankLine as Block =
    lineToBlock 'blank


# | Promote a @Line@ into a @Block@.
lineToBlock as fn Line: Block =
    fn x:
    'singleLine 'noRequiredBreaks (mkIndentedLine x)


textToBlock as fn Text: Fmt.Block =
    fn f: f >> 'text_ >> lineToBlock


# | Promote a @Line@ into a @Block@ that will always have a newline at the end of it,
# meaning that this @Line@ will never have another @Line@ joined to its right side.
mustBreak as fn Line: Block =
    fn x:
    'singleLine 'mustBreakAtEnd (mkIndentedLine x)


mkIndentedLine as fn Line: Indented Line =
    fn l:
    try l as

        'space:
            'indented (indent_spaces 1) 'blank

        'row 'space next:
            'indented i rest_ =
                mkIndentedLine next

            'indented (List.concat [ indent_spaces 1, i ]) rest_

        other:
            'indented [] other


# | A vertical stack of @Block@s.  The left edges of all the @Block@s will be aligned.
stack as fn [ Block ]: Block =
    stackForce as fn Block, Block: Block =
        fn b2, b1:
        toLines as fn Block: [ Indented Line ] =
            fn b:
            try b as

                'empty:
                    []

                'singleLine _ l1:
                    # We lose information about RequiredLineBreaks, but that's okay
                    # since the result will always be a multiline stack,
                    # which will never join with a single line
                    [ l1 ]

                'stack l1 rest:
                    l1 :: rest

        try toLines b1 as
            []: b2
            line1first :: line1rest: 'stack line1first (List.concat [ line1rest, toLines b2 ])

    fn bs:
    if bs == [] then
        'empty
    else
        for1 (List.reverse bs) stackForce


mapLines as fn fn Indented Line: Indented Line, Block: Block =
    fn f, b:
    mapFirstLine f f b


mapFirstLine as fn fn Indented Line: Indented Line, fn Indented Line: Indented Line, Block: Block =
    fn firstFn, restFn, b:
    try b as
        'empty: 'empty
        'singleLine breaks l1: 'singleLine breaks (firstFn l1)
        'stack l1 ls: 'stack (firstFn l1) (List.map ls restFn)


mapLastLine as fn fn Indented Line: Indented Line, Block: Block =
    fn lastFn, b:
    try b as

        'empty:
            'empty

        'singleLine breaks l1:
            'singleLine breaks (lastFn l1)

        'stack l1 ls:
            try List.reverse ls as
                last :: init: 'stack l1 __ << List.reverse [ lastFn last, init... ]
                _: todo "what"


# | Makes a new @Block@ with the contents of the input @Block@ indented by one additional level.
indent as fn Block: Block =
    mapLines (fn 'indented i l: 'indented (List.concat [ indent_tab, i ]) l) __


# | This is the same as `rowOrStackForce` @False@.
rowOrStack as fn Maybe Line, [ Block ]: Block =
    rowOrStackForce 'false __ __


# | If all given @Block@s are single-line and the @Bool@ is @False@,
# then makes a new single-line @Block@, with the @Maybe Line@ interspersed.
# Otherwise, makes a vertical `stack` of the given @Block@s.
rowOrStackForce as fn Bool, Maybe Line, [ Block ]: Block =
    fn forceMultiline, joiner, blocks:
    try blocks as

        [ single ]:
            single

        _:
            if forceMultiline then
                stack blocks
            else
                try maybeAllSingleLines blocks as

                    'just (lines & mkLine):
                        try joiner as
                            'nothing: for1 lines (fn a, b: 'row b a)
                            'just j: for1 (List.intersperse j lines) (fn a, b: 'row b a)
                        >> mkLine

                    _:
                        stack blocks


# | Same as `rowOrIndentForce` @False@.
rowOrIndent as fn Maybe Line, [ Block ]: Block =
    rowOrIndentForce 'false __ __


# | This is the same as `rowOrStackForce`, but all non-first lines in
# the resulting block are indented one additional level.
rowOrIndentForce as fn Bool, Maybe Line, [ Block ]: Block =
    fn forceMultiline, joiner, blocks:
    try blocks as

        []:
            todo "blocks is supposed to be NonEmpty"

        [ single ]:
            single

        [ b1, rest... ]:
            try maybeAllSingleLines blocks as

                'just (reversedLines & mkLine):
                    if forceMultiline then
                        stack (b1 :: List.map rest indent)
                    else
                        # TODO this seems to be repeated above.
                        try joiner as
                            'nothing: for1 reversedLines (fn a, b: 'row b a)
                            'just j: for1 (List.intersperse j reversedLines) (fn a, b: 'row b a)
                        >> mkLine

                _:
                    stack (b1 :: List.map rest indent)


# TODO this outputs the lines in reverse order, it shouldn't!
maybeAllSingleLines as fn [ Block ]: Maybe ([ Line ] & (fn Line: Block)) =
    rec as fn [ Block ], [ Line ]: Maybe ([ Line ] & (fn Line: Block)) =
        fn blocks, reversedLines:
        try blocks as

            []:
                reversedLines & lineToBlock >> 'just

            block :: rest:
                try block as

                    'singleLine 'noRequiredBreaks ('indented _ l):
                        rec rest (l :: reversedLines)

                    'singleLine 'mustBreakAtEnd ('indented _ l):
                        if rest == [] then
                            (l :: reversedLines) & mustBreak >> 'just
                        else
                            'nothing

                    _:
                        'nothing

    rec __ []


# | A convenience alias for `rowOrStack (Just space)`.
spaceSeparatedOrStack as fn [ Block ]: Block =
    rowOrStack ('just space) __


# | A convenience alias for `rowOrStackForce (Just space)`.
spaceSeparatedOrStackForce as fn Bool, [ Block ]: Block =
    fn force, blocks:
    rowOrStackForce force ('just space) blocks


# | A convenience alias for `rowOrIndentForce (Just space)`.
spaceSeparatedOrIndent as fn [ Block ]: Block =
    rowOrIndent ('just space) __


# | A convenience alias for `rowOrIndentForce (Just space)`.
spaceSeparatedOrIndentForce as fn Bool, [ Block ]: Block =
    fn force, blocks:
    rowOrIndentForce force ('just space) blocks


# | Adds the prefix to the first line,
# and pads the other lines with spaces of the given length.
# You are responsible for making sure that the given length is the actual length of the content of the given @Line@.
#
# NOTE: An exceptional case that we haven't really designed for is if the first line of the input Block is indented.
#
# EXAMPLE:
#
# @
# abcde
# xyz
# ----->
# myPrefix abcde
#          xyz
# @
prefix as fn Int, Line, Block: Block =
    fn prefixLength, pref, blocks:
    padLineWithSpaces =
        fn 'indented i l:
        'indented (List.concat [ indent_spaces prefixLength, i ]) l

    addPrefixToLine =
        fn x:
        try x as
            'blank: stripEnd pref
            l: 'row pref l

    mapFirstLine (indent_map addPrefixToLine __) padLineWithSpaces blocks


stripEnd as fn Line: Line =
    fn l:
    try l as

        'space:
            'blank

        'row r1 r2:
            try stripEnd r2 as
                'blank: stripEnd r1
                r2_: 'row r1 r2_

        _:
            l


# | Adds the given suffix to then end of the last line of the @Block@.
# TODO: rename to `suffix`?
addSuffix as fn Line, Block: Block =
    fn suffix, block:
    mapLastLine (indent_map ('row __ suffix) __) block


renderIndentedLine as fn Indented Line: Text =
    fn 'indented i line_:
    line_
    >> stripEnd
    >> renderLine i __
    >> Text.trimRight
    >> __ .. "\n"


spaces as fn Int: Text =
    Text.repeat __ " "


renderLine as fn Indent, Line: Text =
    fn i, l:
    try l as
        'text_ text: spaces (indent_width i) .. text
        'commentWithIndent text: spaces (indent_width i) .. text
        'commentIgnoreIndent text: text
        'space: spaces (1 + indent_width i)
        'row left right: renderLine i left .. renderLine [] right
        'blank: ""


# | Converts a @Block@ into a `Data.ByteString.Builder.Builder`.
#
# You can then write to a file with `Data.ByteString.Builder.writeFile`,
# or convert to @Text@ with @Data.Text.Encoding.decodeUtf8@ . `Data.ByteString.Builder.toLazyByteString`
render as fn Block: Text =
    fn block:
    try block as

        'empty:
            ""

        'singleLine _ line_:
            renderIndentedLine line_

        'stack l1 rest:
            [ l1, rest... ]
            >> List.map __ renderIndentedLine
            >> Text.join "" __
