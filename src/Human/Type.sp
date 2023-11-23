
rootToPath as fn Meta.RootDirectory: Text =
    try __ as
        Meta.'core: "core:"
        Meta.'user: "user:"
        Meta.'installed: "installed:"


importsPathToText as fn Meta.ImportsPath: Text =
    fn Meta.'importsPath root importsDir:

    rootToPath root .. importsDir


umrToText as fn Imports, UMR: Text =
    fn projectImports, 'UMR importsPath sourceDir modulePath:

    importsPathToText importsPath .. "/" .. sourceDir .. "/" .. modulePath


usrToText as fn Imports, USR: Text =
    fn projectImports, 'USR umr name:
    # TODO use display umr if name is not in Imports
    umrToText projectImports umr .. "." .. name


doUsr as fn Imports, USR: FA.Expression =
    fn projectImports, usr:
    {
    , maybeModule = 'nothing
    , name = usrToText projectImports usr
    }
    >> FA.'uppercase
    >> toExpression


#
#
#

uniToText as fn Imports, Uniqueness: Text =
    fn projectImports, uni:
    try uni as
        'imm: ""
        'uni: "!"
        'depends n: Text.fromNumber n .. "?"


#
# Raw
#
toExpression as fn FA.Expr_: FA.Expression =
    FA.'expression [] Pos.'g __


doRawType as fn Imports, TA.RawType: FA.Expression =
    fn projectImports, rawType:
    try rawType as

        TA.'typeExact usr args:
            FA.'call (doUsr projectImports usr) (List.map (doRawType projectImports __) args)

        TA.'typeFn parTypes full:
            FA.'fn FA.'inline (List.map (doParType projectImports __) parTypes) (doFullType projectImports full)

        TA.'typeVar tyvarId:
            doTyvarId projectImports tyvarId

        TA.'typeRecord maybeExtId taAttrs:
            maybeExtension =
                try maybeExtId as
                    'nothing: 'nothing
                    'just id: doTyvarId projectImports id >> toExpression >> 'just >> 'just

            attrs =
                taAttrs
                >> Dict.toList
                >> List.sortBy Tuple.first __
                >> List.map (fn name & raw: { maybeExpr = 'just (doRawType projectImports raw), name = doLowercase projectImports name }) __

            # TODO display tuples as tuples!

            FA.'record { attrs, isMultiline = 'false, maybeExtension }

        TA.'typeError:
            # TODO remove quotes?
            FA.'literalText Token.'singleQuote "???"

        wtf:
            todo "bug: this should not be a type"
    >> toExpression


doTyvarId as fn Imports, TA.TyvarId: FA.Expr_ =
    fn projectImports, tyvarId:
    {
    , attrPath = []
    , maybeModule = 'nothing
    , maybeType = 'nothing
    # TODO use Imports to get original name!
    , name =
        Text.fromNumber tyvarId
    }
    >> FA.'lowercase


doLowercase as fn Imports, Name: FA.Expression =
    fn projectImports, name:
    {
    , attrPath = []
    , maybeModule = 'nothing
    , maybeType = 'nothing
    , name
    }
    >> FA.'lowercase
    >> toExpression


doFullType as fn Imports, TA.FullType: FA.Expression =
    fn projectImports, { raw, uni }:
    FA.'poly (uniToText projectImports uni) (doRawType projectImports raw) >> toExpression


doParType as fn Imports, TA.ParType: FA.Expression =
    fn projectImports, parType:
    try parType as

        TA.'parSp full:
            doFullType projectImports full

        TA.'parRe raw:
            raw
            >> doRawType projectImports __
            >> FA.'unopCall Op.'unopRecycle __
            >> toExpression
