

### LIST

list_intersperse as fn a, [a], [a]: [a] =
    fn separator, items, acc:

    try items as
        , []:
            List.reverse acc

        , [last]:
            List.reverse (last :: acc)

        , head :: tail:
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



display as fn Text, TextTree: Text =
    fn indent, tree:

    try tree as
        , Span _ content:
            indent .. content

        , Block NotIndented content:
            content
            >> List.map (display indent __) __
            >> Text.join "\n" __

        , Block (IndentedWithHeader header) content:

            head =
                indent .. header

            tail =
                content
                >> List.map (display (indent .. "    ") __) __

            head :: tail >> Text.join "\n" __


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



#####


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

    try maybeSpan 0 [] (text (header .. " ") :: content) as
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

    try maybeSpan 0 [] (list_intersperse (text separator) content []) as
        , Just span:
            span

        , Nothing:
            content
            >> List.map (fn c: rowOrIndented separator [c]) __
            >> Block NotIndented __



list as fn { open as Text, separator as Text, close as Text, items as [TextTree]}: TextTree =
    fn ({ open, separator, close, items }):

    zero = Text.length open + Text.length close

    try maybeSpan zero [open .. " "] (list_intersperse (text separator) items []) as
        , Just (Span com t):
            Span com (t .. close)

        , _:
            [
            , [text open]
            , items >> List.map (fn c: rowOrHead separator [c]) __
            , [text << " " .. close]
            ]
            >> List.concat
            >> Block NotIndented __




alias Env =
    {
    }


tyvarIdToText as fn Env, TA.TyvarId: Text =
    fn env, id:

    # TODO translate to original tyvar name?
    Text.fromNumber id


usrToText as fn Env, USR: Text =
    fn env, usr:

    USR umr name = usr
    # TODO use display umr if name is not in modules.sp
    name


uniToText as fn Env, Uniqueness: Text =
    fn env, uni:

    try uni as
        , Imm: ""
        , Uni: "!"
        , Depends n: Text.fromNumber n .. "?"


doRawType as fn Env, TA.RawType: TextTree =
    fn env, raw:

    try raw as
        , TA.TypeUnion maybeExt cons:
            [
            , "<"
            , try maybeExt as
                  , Nothing: ""
                  , Just ext: Text.fromNumber ext .. " with"
            , cons
                >> Dict.toList
                >> List.map (fn n & args: n .. (List.map Debug.toHuman args >> Text.join " " __)) __
                >> Text.join ", " __
            , ">"
            ]
            >> Text.join " " __
            >> text

        , TA.TypeOpaque usr pars:
            # TODO pars!!!
            Debug.toHuman usr
            >> text

        , TA.TypeFn parTypes full:
            #
            #   fn A, B: C
            #
            #   fn
            #     , A
            #     , B
            #     : C
            #
            [
            , separatedBy "," (List.map (doParType env __) parTypes)
            , rowOrIndented ":" [doFullType env full]
            ]
            >> rowOrIndented "fn" __

        , TA.TypeVar tyvarId:
            tyvarId
            >> Text.fromNumber
            >> text

        , TA.TypeRecord maybeExtId attrs:
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
            doAttr as fn (Name & TA.RawType): TextTree =
                fn (name & r):
                rowOrHead (name .. " as") [doRawType env r]

            open =
                try maybeExtId as
                    , Just tyvarId: "{ " .. tyvarIdToText env tyvarId .. " with"
                    , Nothing: "{"

            list
                {
                , open
                , separator = ","
                , close = "}"
                , items = attrs >> Dict.toList >> List.map doAttr __
                }


        , TA.TypeError:
            text "???"


doFullType as fn Env, TA.FullType: TextTree =
    fn env, ({ uni, raw }):

    # TODO try to keep it in a row
    rowOrIndented (uniToText env uni) [doRawType env raw]


doParType as fn Env, TA.ParType: TextTree =
    fn env, parType:

    try parType as
        , TA.ParSp full: doFullType env full

        # TODO try to keep it in a row
        , TA.ParRe raw: rowOrHead "@" [doRawType env raw]

