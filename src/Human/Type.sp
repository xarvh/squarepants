

# TODO this should allow us to use the aliases and globals defined in modules.sp
alias Env =
    {
    }


tyvarIdToText as fn Env, TA.TyvarId: Text =
    fn env, id:

    # TODO translate to original tyvar name?
    Text.fromNumber id


sourceToText as fn Meta.Source: Text =
    fn source:
    try source as
        , Meta.Core: "core:"
        , Meta.Posix: "posix:"
        , Meta.Browser: "browser:"
        , Meta.SourceDirId text: text .. ":"


umrToText as fn Env, UMR: Text =
    fn env, UMR source modulePath:
    sourceToText source .. modulePath


usrToText as fn Env, USR: Text =
    fn env, USR umr name:

    # TODO use display umr if name is not in modules.sp
    umrToText env umr .. "." .. name


uniToText as fn Env, Uniqueness: Text =
    fn env, uni:

    try uni as
        , Imm: ""
        , Uni: "!"
        , Depends n: Text.fromNumber n .. "?"


doRawType as fn Env, TA.RawType: TextTree =
    fn env, raw:

    try raw as
        , TA.TypeRecursive usr args:
            TT.list {
                , open = "$Rec: " .. usrToText env usr
                , separator = ","
                , close = "$"
                , items = List.map (doRawType env __) args
                }

        , TA.TypeUnion maybeExt cons:
            #
            #   < ext with A x y, B z, C >
            #
            #   < ext with
            #   , A x y
            #   , B z
            #   , C
            #   >
            #
            doCons as fn (Name & [TA.RawType]): TextTree =
                fn (name & args):
                TT.rowOrHead name (List.map (doRawType env __) args)

            open =
                try maybeExt as
                    , Just tyvarId: "< " .. tyvarIdToText env tyvarId .. " with"
                    , Nothing: "<"

            TT.list
                {
                , open
                , separator = ","
                , close = ">"
                , items = cons >> Dict.toList >> List.map doCons __
                }


        , TA.TypeOpaque usr args:
            TT.rowOrIndented
                ("opaque:" .. usrToText env usr)
                (List.map (doRawType env __) args)


        , TA.TypeFn parTypes full:
            #
            #   fn A, B: C
            #
            #   fn
            #     , A
            #     , B
            #     : C
            #
            TT.rowOrIndented "" [
                , TT.list {
                    , open = "fn"
                    , separator = ","
                    , close = ":"
                    , items = List.map (doParType env __) parTypes
                    }
                , doFullType env full
                ]


        , TA.TypeVar tyvarId:
            tyvarId
            >> Text.fromNumber
            >> TT.text

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
                TT.rowOrHead (name .. " as") [doRawType env r]

            open =
                try maybeExtId as
                    , Just tyvarId: "{ " .. tyvarIdToText env tyvarId .. " with"
                    , Nothing: "{"

            TT.list
                {
                , open
                , separator = ","
                , close = "}"
                , items = attrs >> Dict.toList >> List.map doAttr __
                }


        , TA.TypeError:
            TT.text "???"

        , wtf:
            TT.text << "###" .. Debug.toHuman wtf .. "###"


doFullType as fn Env, TA.FullType: TextTree =
    fn env, ({ uni, raw }):

    # TODO try to keep it in a row
    TT.rowOrIndented (uniToText env uni) [doRawType env raw]


doParType as fn Env, TA.ParType: TextTree =
    fn env, parType:

    try parType as
        , TA.ParSp full: doFullType env full

        # TODO try to keep it in a row
        , TA.ParRe raw: TT.rowOrHead "@" [doRawType env raw]

