

### LIST

list_intersperse as a: [a]: [a]: [a] =
    separator: items: acc:

    try items as
        []:
            List.reverse acc

        [last]:
            List.reverse (last :: acc)

        head :: tail:
            list_intersperse separator tail (separator :: head :: acc)





# TEXT TREE


alias Complexity = Int

union IsIndented = NotIndented, IndentedWithHeader Text

union TextTree =
  #
  # Multiple spans can be joined in a single horizontal row, but only until Complexity reaches a certain threshold
  #
  , Span Complexity Text

  #
  # All lines in a block occupy their own rows and will be joined with the rest only vertically
  #
  , Block IsIndented [TextTree]



display as Text: TextTree: Text =
    indent: tree:

    try tree as
        Span _ content:
            indent .. content

        Block NotIndented content:
            content
            >> List.map (display indent)
            >> Text.join "\n"

        Block (IndentedWithHeader header) content:

            head =
                indent .. header

            tail =
                content
                >> List.map (display << indent .. "    ")

            head :: tail >> Text.join "\n"


maybeSpan as Int: [Text]: [TextTree]: Maybe TextTree =
    complexityAcc: textAcc: content:

    if complexityAcc > 20 then
      Nothing
    else
      try content as
          []:
              textAcc
              >> List.reverse
              >> Text.join ""
              >> Span complexityAcc
              >> Just

          Span complexity snippet :: tail:
              maybeSpan (complexity + complexityAcc) (snippet :: textAcc) tail

          _:
              Nothing



#####


text as Text: TextTree =
    text:

    Span (Text.length text) text


stack as [TextTree]: TextTree =
    content:

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
rowOrIndented as Text: [TextTree]: TextTree =
    header: content:

    try maybeSpan 0 [] (text (header .. " ") :: content) as
        Just span:
            span

        Nothing:
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
rowOrHead as Text: [TextTree]: TextTree =
    #TODO Needs a change in the TextTree type
    rowOrIndented


separatedBy as Text: [TextTree]: TextTree =
    separator: content:

    try maybeSpan 0 [] (list_intersperse (text separator) content []) as
        Just span:
            span

        Nothing:
            content
            >> List.map (c: rowOrIndented separator [c])
            >> Block NotIndented




list as { open as Text, separator as Text, close as Text, items as [TextTree]}: TextTree =
    ({ open, separator, close, items }):

    zero = Text.length open + Text.length close

    try maybeSpan zero [open .. " "] (list_intersperse (text separator) items []) as
        Just (Span com t):
            Span com (t .. close)

        _:
            [
            , [text open]
            , items >> List.map (c: rowOrHead separator [c])
            , [text << " " .. close]
            ]
            >> List.concat
            >> Block NotIndented




alias Env =
    {
    }


tyvarIdToText as Env: TA.TyvarId: Text =
    env: id:

    # TODO translate to original tyvar name?
    Text.fromNumber id


usrToText as Env: USR: Text =
    env: usr:

    USR umr name = usr
    # TODO use display umr if name is not in modules.sp
    name


uniToText as Env: Uniqueness: Text =
    env: uni:

    try uni as
        Imm: ""
        Uni: "!"
        Depends n: Text.fromNumber n .. "?"


doRawType as Env: TA.RawType: TextTree =
    env: raw:

    try raw as
        TA.TypeExact usr raws:
            #
            #   TheUsr Arg1 Arg2...
            #
            # or
            #
            #   TheUsr
            #       Arg1
            #       Arg2
            #
            # TODO what about List?
            #
            rowOrIndented
                (usrToText env usr)
                (List.map (doRawType env) raws)

        TA.TypeFn parTypes full:
            #
            #   fn A, B: C
            #
            #   fn
            #     , A
            #     , B
            #     : C
            #
            [
            , separatedBy "," (List.map (doParType env) parTypes)
            , rowOrIndented ":" [doFullType env full]
            ]
            >> rowOrIndented "fn"

        TA.TypeVar tyvarId:
            tyvarId
            >> Text.fromNumber
            >> text

        TA.TypeRecord maybeExtId attrs:
            #
            #   { ext with x as X, y as Y }
            #
            #   { ext with
            #   , x as X
            #   , y as Y
            #   }
            #
            # TODO tuple
            #
            doAttr as (Name & TA.RawType): TextTree =
                (name & raw):
                rowOrHead (name .. " as") [doRawType env raw]

            open =
                try maybeExtId as
                    Just tyvarId: "{ " .. tyvarIdToText env tyvarId .. " with"
                    Nothing: "{"

            list
                {
                , open
                , separator = ","
                , close = "}"
                , items = attrs >> Dict.toList >> List.map doAttr
                }


        TA.TypeError:
            text "???"


doFullType as Env: TA.FullType: TextTree =
    env: ({ uni, raw }):

    # TODO try to keep it in a row
    rowOrIndented (uniToText env uni) [doRawType env raw]


doParType as Env: TA.ParType: TextTree =
    env: parType:

    try parType as
        TA.ParSp full: doFullType env full

        # TODO try to keep it in a row
        TA.ParRe raw: rowOrHead "@" [doRawType env raw]

