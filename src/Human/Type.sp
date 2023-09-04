# TODO this should allow us to use the aliases and globals defined in modules.sp
alias Env =
    Compiler/TypeCheck.Env


#
# Meta
#

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


doUsr as fn Env, USR: FA.Expression =
    fn env, usr:
    {
    , maybeType = Nothing
    , tokenWord =
        {
        , attrPath = []
        , isUpper = True
        , maybeModule = Nothing
        , modifier = Token.NameNoModifier
        , name = usrToText env usr
        }
    }
    >> FA.Variable
    >> toExpression


#
#
#

uniToText as fn Env, Uniqueness: Text =
    fn env, uni:
    try uni as
        , Imm: ""
        , Uni: "!"
        , Depends n: Text.fromNumber n .. "?"


#
# Raw
#
toExpression as fn FA.Expr_: FA.Expression =
    FA.Expression [] Pos.G __


doRawType as fn Env, TA.RawType: FA.Expression =
    fn env, rawType:
    try rawType as

        , TA.TypeExact usr args:
            FA.Call (doUsr env usr) (List.map (doRawType env __) args)

        , TA.TypeFn parTypes full:
            FA.Fn FA.Inline (List.map (doParType env __) parTypes) (doFullType env full)

        , TA.TypeVar tyvarId:
            doTyvarId env tyvarId

        , TA.TypeRecord maybeExtId taAttrs:
            maybeExtension =
                try maybeExtId as
                    , Nothing: Nothing
                    , Just id: doTyvarId env id >> toExpression >> Just >> Just

            attrs =
                taAttrs
                >> Dict.toList
                >> List.sortBy Tuple.first __
                >> List.map (fn name & raw: { maybeExpr = Just (doRawType env raw), name = doName env name }) __

            # TODO display tuples as tuples!

            FA.Record { attrs, isMultiline = False, maybeExtension }

        , TA.TypeError:
            todo "TypeError"

        , wtf:
            todo "bug: this should not be a type"
    >> toExpression


doTyvarId as fn Env, TA.TyvarId: FA.Expr_ =
    fn env, tyvarId:
    {
    , maybeType = Nothing
    , tokenWord =
        {
        , attrPath = []
        , isUpper = False
        , maybeModule = Nothing
        , modifier = Token.NameNoModifier
        # TODO use Env to get original name!
        , name =
            Text.fromNumber tyvarId
        }
    }
    >> FA.Variable


doName as fn Env, Name: FA.Expression =
    fn env, name:
    {
    , maybeType = Nothing
    , tokenWord =
        {
        , attrPath = []
        , isUpper = False
        , maybeModule = Nothing
        , modifier = Token.NameNoModifier
        , name
        }
    }
    >> FA.Variable
    >> toExpression


doFullType as fn Env, TA.FullType: FA.Expression =
    fn env, { raw, uni }:
    FA.Poly (uniToText env uni) (doRawType env raw) >> toExpression


doParType as fn Env, TA.ParType: FA.Expression =
    fn env, parType:
    try parType as

        , TA.ParSp full:
            doFullType env full

        , TA.ParRe raw:
            raw
            >> doRawType env __
            >> FA.UnopCall Op.UnopRecycle __
            >> toExpression
