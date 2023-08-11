#
# This module is a verbatim translation of https://codeberg.org/avh4/prettyprint-avh4/src/branch/main/src/Text/PrettyPrint/Avh4/
# (Both Block.hs and Indent.hs have been merged here)
#




# I'm too tired to use NonEmpty
for1 as fn [a], (fn a, a: a): a =
    fn items, f:
    try items as
        , []: todo "for1 got an empty list...."
        , [head, ...tail]: List.for head tail f





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
alias Indent =
    [Int]

#instance Eq Indent where
#  a == b =
#    width' a == width' b

indent_tab as Indent =
    [spacesInTab]

indent_spaces as fn Int: Indent =
    fn n:
    [n]


indent_width as fn Indent: Int =
    List.for 0 __ indent_combine


indent_combine as fn Int, Int: Int =
  fn pos, i:
  if i < spacesInTab then
    # The right side starts with spaces (and no TABs),
    # so just add everything together.
    pos + i
  else
    # The right side starts with at least one TAB,
    # so remove the trailing spaces from the left.
    pos - (modBy spacesInTab pos) + i




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
union Line =
  , Text_ Text
  , Row Line Line
  , Space
  , Blank


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
    Space


union Indented a =
  , Indented Indent a


indent_map as fn (fn Line: Line), Indented Line: Indented Line =
    fn f, Indented indent l:
    Indented indent (f l)


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
union Block =
  , SingleLine RequiredLineBreaks (Indented Line)
  , Stack (Indented Line) [Indented Line]

union RequiredLineBreaks =
  , MustBreakAtEnd
  , NoRequiredBreaks

# | A blank line (taking up one vertical space), with no text content.
blankLine as Block =
  lineToBlock Blank

# | Promote a @Line@ into a @Block@.
lineToBlock as fn Line: Block =
  fn x:
  SingleLine NoRequiredBreaks (mkIndentedLine x)


textToBlock as fn Text: Fmt.Block =
    fn f: f >> Text_ >> lineToBlock


# | Promote a @Line@ into a @Block@ that will always have a newline at the end of it,
# meaning that this @Line@ will never have another @Line@ joined to its right side.
mustBreak as fn Line: Block =
  fn x:
  SingleLine MustBreakAtEnd (mkIndentedLine x)

mkIndentedLine as fn Line: Indented Line =
    fn l:
    try l as
        , Space:
            Indented (indent_spaces 1) Blank
        , Row Space next:
            Indented i rest_ = mkIndentedLine next
            Indented (List.concat [indent_spaces 1, i]) rest_
        , other:
            Indented [] other


# | A vertical stack of @Block@s.  The left edges of all the @Block@s will be aligned.
stack as fn [Block]: Block =

    stackForce as fn Block, Block: Block =
        fn b1, b2:

        toLines as fn Block: [Indented Line] =
            fn b:
            try b as

              , SingleLine _ l1:
                  # We lose information about RequiredLineBreaks, but that's okay
                  # since the result will always be a multiline stack,
                  # which will never join with a single line
                  [l1]

              , Stack l1 rest:
                  l1 :: rest

        line1first & line1rest =
            try toLines b1 as
                , []: todo "lk"
                , head :: tail: head & tail

        Stack line1first (List.concat [ line1rest, toLines b2])


    fn bs:
    for1 (List.reverse bs) stackForce



mapLines as fn (fn Indented Line: Indented Line), Block: Block =
  fn f, b:
  mapFirstLine f f b


mapFirstLine as fn (fn Indented Line: Indented Line), (fn Indented Line: Indented Line), Block: Block =
  fn firstFn, restFn, b:
  try b as
    , SingleLine breaks l1:
      SingleLine breaks (firstFn l1)
    , Stack l1 ls:
      Stack (firstFn l1) (List.map restFn ls)


mapLastLine as fn (fn Indented Line: Indented Line), Block: Block =
  fn lastFn, b:
  try b as

    , SingleLine breaks l1:
        SingleLine breaks (lastFn l1)

    , Stack l1 ls:
        try List.reverse ls as
            , last :: init:
                  Stack l1 __ << List.reverse [ lastFn last, ...init ]
            , _:
                todo "what"


# | Makes a new @Block@ with the contents of the input @Block@ indented by one additional level.
indent as fn Block: Block =
  mapLines (fn Indented i l: Indented (List.concat [indent_tab, i]) l) __


# | This is the same as `rowOrStackForce` @False@.
rowOrStack as fn Maybe Line, [Block]: Block =
      rowOrStackForce False __ __

# | If all given @Block@s are single-line and the @Bool@ is @False@,
# then makes a new single-line @Block@, with the @Maybe Line@ interspersed.
# Otherwise, makes a vertical `stack` of the given @Block@s.
rowOrStackForce as fn Bool, Maybe Line, [Block]: Block =
    fn forceMultiline, joiner, blocks:

    try blocks as
        , [single]:
           single
        , _:
            try maybeAllSingleLines blocks as
                , Just (lines & mkLine):
                    if forceMultiline then
                      stack blocks
                    else
                       try joiner as
                            , Nothing: for1 lines Row
                            , Just j: for1 (List.intersperse j lines []) Row
                       >> mkLine

                , _:
                  stack blocks


# | Same as `rowOrIndentForce` @False@.
rowOrIndent as fn Maybe Line, [Block]: Block =
    rowOrIndentForce False __ __


# | This is the same as `rowOrStackForce`, but all non-first lines in
# the resulting block are indented one additional level.
rowOrIndentForce as fn Bool, Maybe Line, [Block]: Block =
    fn forceMultiline, joiner, blocks:

    try blocks as
        , []:
            todo "blocks is supposed to be NonEmpty"

        , [single]:
            single

        , [b1, ...rest]:
            try maybeAllSingleLines blocks as
                , Just (reversedLines & mkLine):
                    if forceMultiline then
                      stack (b1 :: (List.map indent rest))
                    else
                        # TODO this seems to be repeated above.
                        try joiner as
                            , Nothing: for1 reversedLines Row
                            , Just j: for1 (List.intersperse j reversedLines []) Row
                        >> mkLine

                , _:
                  stack (b1 :: (List.map indent rest))


maybeAllSingleLines as fn [Block]: Maybe ([Line] & fn Line: Block ) =

    rec as fn [Block], [Line]: Maybe ([Line] & fn Line: Block) =
        fn blocks, reversedLines:

        try blocks as
            , []:
                reversedLines & lineToBlock
                >> Just

            , block :: rest:
                try block as
                    , SingleLine NoRequiredBreaks (Indented _ l):
                        rec rest (l :: reversedLines)

                    , SingleLine MustBreakAtEnd (Indented _ l):
                        if rest == [] then
                            (l :: reversedLines) & mustBreak
                            >> Just
                        else
                            Nothing

                    , _:
                        Nothing

    rec __ []


# | A convenience alias for `rowOrStack (Just space)`.
spaceSeparatedOrStack as fn [Block]: Block =
    rowOrStack (Just space) __

# | A convenience alias for `rowOrStackForce (Just space)`.
spaceSeparatedOrStackForce as fn Bool, [Block]: Block =
    fn force, blocks:
    rowOrStackForce force (Just space) blocks

# | A convenience alias for `rowOrIndentForce (Just space)`.
spaceSeparatedOrIndent as fn [Block]: Block =
   rowOrIndent (Just space) __

# | A convenience alias for `rowOrIndentForce (Just space)`.
spaceSeparatedOrIndentForce as fn Bool, [Block]: Block =
    fn force, blocks:
    rowOrIndentForce force (Just space) blocks

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
      fn Indented i l:
      Indented (List.concat [indent_spaces prefixLength, i]) l

  addPrefixToLine =
      fn x:
      try x as
          , Blank: stripEnd pref
          , l: Row pref l

  mapFirstLine (indent_map addPrefixToLine __) padLineWithSpaces blocks


stripEnd as fn Line: Line =
    fn l:
    try l as
      , Space: Blank
      , Row r1 r2:
        try stripEnd r2 as
          , Blank: stripEnd r1
          , r2_: Row r1 r2_
      , Text_ t: Text_ t
      , Blank: Blank

# | Adds the given suffix to then end of the last line of the @Block@.
addSuffix as fn Line, Block: Block =
    fn suffix, block:
    mapLastLine (indent_map (Row __ suffix) __) block


renderIndentedLine as fn Indented Line: Text =
  fn (Indented i line_):
  renderLine i (stripEnd line_) .. "\n"


spaces as fn Int: Text =
    Text.repeat __ " "


renderLine as fn Indent, Line: Text =
  fn i, l:
  try l as
      , Text_ text:
          spaces (indent_width i) .. text
      , Space:
          spaces (1 + indent_width i)
      , Row left right:
          renderLine i left .. renderLine [] right
      , Blank:
          ""

# | Converts a @Block@ into a `Data.ByteString.Builder.Builder`.
#
# You can then write to a file with `Data.ByteString.Builder.writeFile`,
# or convert to @Text@ with @Data.Text.Encoding.decodeUtf8@ . `Data.ByteString.Builder.toLazyByteString`
render as fn Block: Text =
    fn block:
    try block as
        , SingleLine _ line_:
            renderIndentedLine line_
        , Stack l1 rest:
            [l1, ...rest]
            >> List.map renderIndentedLine __
            >> Text.join "" __

