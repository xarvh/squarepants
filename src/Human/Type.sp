rootToPath as fn Meta.RootDirectory: Text =
    try __ as
        Meta.'core: "core:"
        Meta.'user: "user:"
        Meta.'installed: "installed:"


#importsPathToText as fn Meta.ImportsPath: Text =
#    fn Meta.'importsPath root importsDir:
#
#    rootToPath root .. importsDir

umrToText as fn CA.Module, UMR: Text =
    fn contextModule, umr:
    try Dict.get umr contextModule.umrToAlias as

        'just alias:
            alias

        'nothing:
            'UMR rootDirectory sourceDirId modulePath =
                umr

            #rootToPath rootDirectory .. Text.fromNumber sourceDirId .. ":" .. modulePath
            modulePath


usrToText as fn CA.Module, USR: Text =
    fn contextModule, usr:
    try Dict.get usr contextModule.usrToGlobal as

        'just globalName:
            globalName

        'nothing:
            'USR umr name =
                usr

            umrToText contextModule umr .. "." .. name


doUsr as fn CA.Module, USR: FA.Expression =
    fn contextModule, usr:
    {
    , maybeModule = 'nothing
    , name = usrToText contextModule usr
    }
    >> FA.'uppercase
    >> toExpression


#
#
#

uniToText as fn CA.Module, Uniqueness: Text =
    fn contextModule, uni:
    try uni as
        'imm: ""
        'uni: "!"
        'depends n: Text.fromNumber n .. "?"


#
# Raw
#
toExpression as fn FA.Expr_: FA.Expression =
    FA.'expression [] Pos.'g __


doRawType as fn CA.Module, TA.RawType: FA.Expression =
    fn contextModule, rawType:
    try rawType as

        TA.'typeExact _ usr args:
            FA.'call (doUsr contextModule usr) (List.map (doRawType contextModule __) args)

        TA.'typeFn _ _ parTypes full:
            FA.'fn FA.'inline (List.map (doParType contextModule __) parTypes) (doFullType contextModule full)

        TA.'typeVar _ tyvarId:
            doTyvarId contextModule tyvarId

        TA.'typeRecord _ maybeExtId taAttrs:
            maybeExtension =
                try maybeExtId as
                    'nothing: 'nothing
                    'just id: doTyvarId contextModule id >> toExpression >> 'just >> 'just

            attrs =
                taAttrs
                >> Dict.toList
                >> List.sortBy Tuple.first __
                >> List.map (fn name & raw: { maybeExpr = 'just (doRawType contextModule raw), name = doLowercase contextModule name }) __

            # TODO display tuples as tuples!

            FA.'record { attrs, isMultiline = 'false, maybeExtension }

        TA.'typeError:
            # TODO remove quotes?
            FA.'literalText Token.'singleQuote "???"

        wtf:
            todo "bug: this should not be a type"
    >> toExpression


doTyvarId as fn CA.Module, TA.TyvarId: FA.Expr_ =
    fn contextModule, tyvarId:
    {
    , attrPath = []
    , maybeModule = 'nothing
    , maybeType = 'nothing
    # TODO use Imports to get original name!
    , name =
        Text.fromNumber tyvarId
    }
    >> FA.'lowercase


doLowercase as fn CA.Module, Name: FA.Expression =
    fn contextModule, name:
    {
    , attrPath = []
    , maybeModule = 'nothing
    , maybeType = 'nothing
    , name
    }
    >> FA.'lowercase
    >> toExpression


doFullType as fn CA.Module, TA.FullType: FA.Expression =
    fn contextModule, { raw, uni }:
    FA.'poly (uniToText contextModule uni) (doRawType contextModule raw) >> toExpression


doParType as fn CA.Module, TA.ParType: FA.Expression =
    fn contextModule, parType:
    try parType as

        TA.'parSp full:
            doFullType contextModule full

        TA.'parRe raw:
            raw
            >> doRawType contextModule __
            >> FA.'unopCall Op.'unopRecycle __
            >> toExpression
