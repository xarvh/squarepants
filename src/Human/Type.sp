# TODO this should allow us to use the aliases and globals defined in modules.sp
Env =
    Compiler/TypeCheck.Env


#
# Meta
#

sourceToText as fn Meta.Source: Text =
    fn source:
    try source as
        Meta.'core: "core:"


umrToText as fn Env, UMR: Text =
    fn env, 'UMR source modulePath:
    sourceToText source .. modulePath


usrToText as fn Env, USR: Text =
    fn env, 'USR umr name:
    # TODO use display umr if name is not in modules.sp
    umrToText env umr .. "." .. name


doUsr as fn Env, USR: FA.Expression =
    fn env, usr:
    {
    , maybeModule = 'nothing
    , name = usrToText env usr
    }
    >> FA.'uppercase
    >> toExpression


#
#
#

uniToText as fn Env, Uniqueness: Text =
    fn env, uni:
    try uni as
        'imm: ""
        'uni: "!"
        'depends n: Text.fromNumber n .. "?"


#
# Raw
#
toExpression as fn FA.Expr_: FA.Expression =
    FA.'expression [] Pos.'g __


doRawType as fn Env, TA.RawType: FA.Expression =
    fn env, rawType:
    try rawType as

        TA.'typeExact usr args:
            FA.'call (doUsr env usr) (List.map (doRawType env __) args)

        TA.'typeFn parTypes full:
            FA.'fn FA.'inline (List.map (doParType env __) parTypes) (doFullType env full)

        TA.'typeVar tyvarId:
            doTyvarId env tyvarId

        TA.'typeRecord maybeExtId taAttrs:
            maybeExtension =
                try maybeExtId as
                    'nothing: 'nothing
                    'just id: doTyvarId env id >> toExpression >> 'just >> 'just

            attrs =
                taAttrs
                >> Dict.toList
                >> List.sortBy Tuple.first __
                >> List.map (fn name & raw: { maybeExpr = 'just (doRawType env raw), name = doLowercase env name }) __

            # TODO display tuples as tuples!

            FA.'record { attrs, isMultiline = 'false, maybeExtension }

        TA.'typeError:
            # TODO remove quotes?
            FA.'literalText Token.'singleQuote "???"

        wtf:
            todo "bug: this should not be a type"
    >> toExpression


doTyvarId as fn Env, TA.TyvarId: FA.Expr_ =
    fn env, tyvarId:
    {
    , attrPath = []
    , maybeModule = 'nothing
    , maybeType = 'nothing
    # TODO use Env to get original name!
    , name =
        Text.fromNumber tyvarId
    }
    >> FA.'lowercase


doLowercase as fn Env, Name: FA.Expression =
    fn env, name:
    {
    , attrPath = []
    , maybeModule = 'nothing
    , maybeType = 'nothing
    , name
    }
    >> FA.'lowercase
    >> toExpression


doFullType as fn Env, TA.FullType: FA.Expression =
    fn env, { raw, uni }:
    FA.'poly (uniToText env uni) (doRawType env raw) >> toExpression


doParType as fn Env, TA.ParType: FA.Expression =
    fn env, parType:
    try parType as

        TA.'parSp full:
            doFullType env full

        TA.'parRe raw:
            raw
            >> doRawType env __
            >> FA.'unopCall Op.'unopRecycle __
            >> toExpression
