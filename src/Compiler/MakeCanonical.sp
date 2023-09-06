[#
    `Env` is immutable and depends only on the parent scopes.
    Which means, each scope will have a different one.

    `State` is mutable and is reset per each module.
    We don't have state for now.

#]
alias Env =
    {
    # This is the innermost record we're updating, which is what the shorthands will apply to:
    #
    #     new = { old with x = .x + 1 }
    #
    , maybeShorthandTarget as Maybe CA.Expression
    , nextGeneratedVariableName as Int
    , nonFn as Dict Name Pos
    #
    # TODO This is used to tell the user that definitions must be in order
    #
    #, futureNonRootValues as Dict Text Pos
    #
    #
    , ro as ReadOnly
    #
    # Non-root values don't need to be expanded with the module name.
    #
    , values as Dict Name { isRoot as Bool, pos as Pos }
    }


alias ReadOnly =
    {
    , errorModule as Error.Module
    , meta as Meta
    , umr as UMR
    }


initEnv as fn ReadOnly: Env =
    fn ro:
    {
    , maybeShorthandTarget = Nothing
    , nextGeneratedVariableName = 0
    , nonFn = Dict.empty
    #, futureNonRootValues = Dict.empty
    , ro =
        ro
    , values = Dict.empty
    }


#
# Errors
#

erroro as fn ReadOnly, Pos, [ Text ]: Res a =
    fn ro, pos, msg:
    Error.res ro.errorModule pos msg


error as fn Env, Pos, [ Text ]: Res a =
    fn env, pos, msg:
    Error.res env.ro.errorModule pos msg


#
# Names resolution
#

maybeForeignUsr as fn fn Meta: Dict Text USR, ReadOnly, Pos, Maybe Name, Name: Res (Maybe USR) =
    fn getter, ro, pos, maybeModule, name:
    try maybeModule as

        , Nothing:
            Dict.get name (getter ro.meta) >> Ok

        , Just moduleName:
            try Dict.get moduleName ro.meta.moduleVisibleAsToUmr as

                , Just umr:
                    USR umr name
                    >> Just
                    >> Ok

                , Nothing:
                    # For now we're assuming that ro.meta.moduleVisibleAsToUmr contains *all* modules, aliased or not
                    erroro ro pos [ "I can't find the module `" .. moduleName .. "`" ]


#                    (USR Meta.SourcePlaceholder moduleName name)
#
#                    List.each (Dict.keys ro.meta.moduleVisibleAsToUmr) x:
#                        log "*" x
#                    todo << "!!resolveToUsr can't find the module: " .. moduleName .. " (for: " .. name .. ")"

resolveToUsr as fn fn Meta: Dict Text USR, ReadOnly, Pos, Maybe Name, Name: Res USR =
    fn getter, ro, pos, maybeModule, name:
    maybeForeignUsr getter ro pos maybeModule name >> Result.map (Maybe.withDefault (USR ro.umr name) __) __


resolveToValueRef as fn ReadOnly, Pos, Bool, Maybe Name, Name: Res Ref =
    fn ro, pos, isRoot, maybeModule, name:
    # TODO use Result.map?
    try maybeForeignUsr (fn m: m.globalValues) ro pos maybeModule name as

        , Err e:
            Err e

        , Ok (Just usr):
            RefGlobal usr >> Ok

        , Ok Nothing:
            if isRoot then
                USR ro.umr name
                >> RefGlobal
                >> Ok
            else
                RefLocal name >> Ok


resolveToTypeUsr as fn ReadOnly, Pos, Maybe Name, Name: Res USR =
    resolveToUsr (fn m: m.globalTypes) __ __ __ __


resolveToConstructorUsr as fn ReadOnly, Pos, Maybe Name, Name: Res USR =
    resolveToUsr (fn m: m.globalValues) __ __ __ __


#
# Dependencies
#

typeDeps as fn CA.RawType, Set USR: Set USR =
    fn type, acc:
    try type as

        , CA.TypeNamed _ usr args:
            acc >> Set.insert usr __ >> List.for __ args typeDeps

        , CA.TypeAnnotationVariable _ _:
            acc

        , CA.TypeRecord _ attrs:
            Dict.for acc attrs (fn k, v, a: typeDeps v a)

        , CA.TypeError _:
            acc

        , CA.TypeFn _ params to:
            acc
            >> typeDeps to.raw __
            >> List.for __ params fn par, z:
                try par as
                    , CA.ParRe raw: typeDeps raw z
                    , CA.ParSp full: typeDeps full.raw z


alias Deps =
    {
    , cons as Set USR
    , types as Set USR
    , values as Set USR
    }


deps_init =
    {
    , cons = Set.empty
    , types = Set.empty
    , values = Set.empty
    }


patternDeps as fn CA.Pattern, Deps: Deps =
    fn pattern, deps:
    try pattern as
        , CA.PatternConstructor _ usr ps: List.for { deps with cons = Set.insert usr .cons } ps patternDeps
        , CA.PatternRecord _ completeness ps: Dict.for deps ps (fn k, v, a: patternDeps v a)
        , CA.PatternAny _ _ (Just annotation): { deps with types = typeDeps annotation.raw .types }
        , CA.PatternAny _ _ Nothing: deps
        , CA.PatternLiteralNumber _ _: deps
        , CA.PatternLiteralText _ _: deps


expressionDeps as fn CA.Expression, Deps: Deps =
    fn expression, deps:
    try expression as

        , CA.LiteralNumber _ _:
            deps

        , CA.LiteralText _ _:
            deps

        , CA.Variable _ (RefGlobal usr):
            { deps with values = Set.insert usr .values }

        , CA.Variable _ _:
            deps

        , CA.Constructor _ usr:
            { deps with cons = Set.insert usr .cons }

        , CA.Fn _ pars body:
            deps
            >> List.for __ pars parameterDeps
            >> expressionDeps body __

        , CA.Record _ Nothing exprByName:
            Dict.for deps exprByName (fn name, v, a: expressionDeps v a)

        , CA.Record _ (Just expr) exprByName:
            deps
            >> expressionDeps expr __
            >> Dict.for __ exprByName (fn name, v, a: expressionDeps v a)

        , CA.Record _ _ exprByName:
            Dict.for deps exprByName (fn name, v, a: expressionDeps v a)

        , CA.RecordAccess _ _ e:
            expressionDeps e deps

        , CA.Call _ e0 args:
            deps
            >> expressionDeps e0 __
            >> List.for __ args argumentDeps

        , CA.If _ args:
            deps
            >> expressionDeps args.condition __
            >> expressionDeps args.true __
            >> expressionDeps args.false __

        , CA.Try _ { patternsAndExpressions, value }:
            deps
            >> expressionDeps value __
            >> List.for __ patternsAndExpressions (fn u & p & b, d: d >> patternDeps p __ >> expressionDeps b __)

        , CA.LetIn valueDef e:
            deps
            >> patternDeps valueDef.pattern __
            >> expressionDeps valueDef.body __
            >> expressionDeps e __


argumentDeps as fn CA.Argument, Deps: Deps =
    fn arg, deps:
    try arg as
        , CA.ArgumentExpression e: expressionDeps e deps
        , CA.ArgumentRecycle _ _ _: deps


parameterDeps as fn CA.Parameter, Deps: Deps =
    fn par, deps:
    try par as
        , CA.ParameterPattern _ pa: patternDeps pa deps
        , _: deps


#
# Definition
#

translateDefinition as fn Bool, Env, FA.ValueDef: Res (Env & CA.ValueDef) =
    fn isRoot, env, fa:
    nonFn =
        fa.nonFn
        >> List.map (fn pos & name: name & pos) __
        >> Dict.fromList

    fa.pattern
    >> translateFullPattern { env with nonFn } __
    >> onOk fn uni & pattern:
    # TODO: todo: check that the typeclasses are consistent with those declared in parentEnv

    # TODO: check that nonFn contains only names actually used in the annotation?

    env
    >> insertPatternNames isRoot pattern __
    >> onOk fn localEnv:
    fa.body
    >> translateExpression localEnv __
    >> onOk fn body:
    deps =
        if isRoot then
            deps_init >> patternDeps pattern __ >> expressionDeps body __
        else
            deps_init

    localEnv
    & {
    , body
    , directConsDeps = deps.cons
    #
    , directTypeDeps =
        deps.types
    , directValueDeps = deps.values
    , native = False
    , pattern
    , uni
    }
    >> Ok


#
# Pattern
#
translateAttributeName as fn ReadOnly, FA.Expression: Res (Pos & Name & Maybe FA.Expression) =
    fn ro, FA.Expression _ pos expr_:
    try expr_ as

        , FA.Lowercase { attrPath, maybeModule, maybeType, name }:
            if maybeModule /= Nothing then
                erroro ro pos [ "Attribute names must be single words" ]
            else if attrPath /= [] then
                erroro ro pos [ "Attribute names can't contain dots" ]
            else
                pos & name & maybeType >> Ok

        , _:
            erroro ro pos [ "Expecting an attribute name here" ]


translatePatternConstructor as fn Env, Pos, Maybe Name, Name, [ CA.Pattern ]: Res CA.Pattern =
    fn env, pos, maybeModule, name, args:
    resolveToConstructorUsr env.ro pos maybeModule name
    >> onOk fn usr:
    CA.PatternConstructor pos usr args >> Ok


# TODO too many functions args, use named arguments
translatePatternAny as fn Env, Pos, Maybe FA.Expression, Maybe Name, Name, [ Name ]: Res CA.Pattern =
    fn env, pos, maybeType, maybeModule, name, attrPath:
        if attrPath /= [] then
            error env pos [ "pattern names can't have type attributes" ]
        else
            translateMaybeAnnotation env maybeType
            >> onOk fn maybeAnnotation:
            maybeName =
                if name == "_" then Nothing else Just name

            Ok << CA.PatternAny pos maybeName maybeAnnotation


translateMaybeAnnotation as fn Env, Maybe FA.Expression: Res (Maybe CA.Annotation) =
    fn env, maybeFaType:
    try maybeFaType as

        , Nothing:
            Ok Nothing

        , Just faType:
            translateRawType env.ro faType
            >> onOk fn raw:
            tyvars =
                CA.typeTyvars raw >> Dict.map (fn tyvarName, pos: { nonFn = Dict.get tyvarName env.nonFn }) __

            { raw, tyvars, univars = CA.typeUnivars raw }
            >> Just
            >> Ok


insertPatternRecordAttribute as fn Env, FA.RecordAttribute, Dict Name CA.Pattern: Res (Dict Name CA.Pattern) =
    fn env, attr, caAttrs:
    # { x }
    # { x = pattern }
    # { x as Type }

    translateAttributeName env.ro attr.name
    >> onOk fn pos & caName & maybeFaType:
    if Dict.member caName caAttrs then
        error env pos [ "duplicate attribute name in pattern: " .. caName ]
    else
        try attr.maybeExpr & maybeFaType as

            , Just _ & Just (FA.Expression _ typePos _):
                error env typePos [ "if you want to annotate the attribute, use { x = y as TheType }" ]

            , Nothing & Just faType:
                error env pos [ "TODO annotating record attributes needs more thinking" ]

#                translateRawType env.ro faType
#                >> onOk fn caType:
#
#                caAttrs
#                >> Dict.insert caName (CA.PatternAny pos { maybeName = Just caName, maybeAnnotation = Just caType }) __
#                >> Ok

            , Just faPattern
            & Nothing:
                faPattern
                >> translateRawPattern env __
                >> onOk fn caPattern:
                caAttrs
                >> Dict.insert caName caPattern __
                >> Ok

            , Nothing & Nothing:
                caAttrs
                >> Dict.insert caName (CA.PatternAny pos (Just caName) Nothing) __
                >> Ok


translatePatternRecord as fn Env, Pos, Maybe (Maybe FA.Expression), [ { maybeExpr as Maybe FA.Expression, name as FA.Expression } ]: Res CA.Pattern =
    fn env, pos, maybeMaybeExt, attrs:
    try maybeMaybeExt as

        , Just (Just (FA.Expression _ p expr_)):
            error env p [ "Can't extend patterns" ]

        , Just Nothing:
            # { with attr1 = ... }
            Ok CA.Partial

        , Nothing:
            # { attr1 = ... }
            Ok CA.Complete
    >> onOk fn completeness:
    Dict.empty
    >> List.forRes __ attrs (insertPatternRecordAttribute env __ __)
    >> Result.map (fn x: CA.PatternRecord pos completeness x) __


translateTuple as fn ReadOnly, fn FA.Expression: Res ca, FA.BinopChain: Res (Dict Name ca) =
    fn ro, translate, chain:
    faExpressions as [ FA.Expression ] =
        FA.binopChainExpressions chain

    faExpressions
    >> List.mapRes translate __
    >> onOk fn items:
    pos as Pos =
        List.for Pos.G faExpressions (fn FA.Expression _ p _, z: Pos.range p z)

    try items as

        , [ ca1, ca2 ]:
            Dict.empty
            >> Dict.insert "first" ca1 __
            >> Dict.insert "second" ca2 __
            >> Ok

        , [ ca1, ca2, ca3 ]:
            Dict.empty
            >> Dict.insert "first" ca1 __
            >> Dict.insert "second" ca2 __
            >> Dict.insert "third" ca3 __
            >> Ok

        , _:
            erroro ro pos [ "tuples can be only of size 2 or 3, use a record instead" ]


translateFullPattern as fn Env, FA.Expression: Res (Uniqueness & CA.Pattern) =
    fn env, expr:
    expr
    >> translatePoly env.ro __
    >> onOk fn uni & e:
    translateRawPattern env e
    >> onOk fn caPa:
    uni & caPa >> Ok


translateRawPattern as fn Env, FA.Expression: Res CA.Pattern =
    fn env, FA.Expression _ pos expr_:
    try expr_ as

        , FA.Constructor { maybeModule, name }:
            translatePatternConstructor env pos maybeModule name []

        , FA.Lowercase { attrPath, maybeModule, maybeType, name }:
            translatePatternAny env pos maybeType maybeModule name attrPath

        , FA.Call (FA.Expression _ p ref) faArgs:
            try ref as

                , FA.Constructor { maybeModule, name }:
                    faArgs
                    >> List.mapRes (translateRawPattern env __) __
                    >> onOk fn caPars:
                    translatePatternConstructor env pos maybeModule name caPars

                , _:
                    error env p [ "I was expecting a constructor name here" ]

        , FA.List _ faItems:
            reversedFaItems =
                List.reverse faItems

            pushItem as fn CA.Pattern, CA.Pattern: CA.Pattern =
                fn pattern, last:
                CA.PatternConstructor (CA.patternPos pattern) CoreTypes.cons [ pattern, last ]

            try reversedFaItems as

                , []:
                    CA.PatternConstructor pos CoreTypes.nil [] >> Ok

                , [ lastHasDots & FA.Expression _ p lastFaExpr, ...reversedFaRest ]:
                    if List.any Tuple.first reversedFaRest then
                        error env p [ "only the last item in a list can have ... triple dots" ]
                    else if not lastHasDots then
                        reversedFaItems
                        >> List.mapRes (fn hasDots & expr: translateRawPattern env expr) __
                        >> onOk fn reversedCaItems:
                        List.for (CA.PatternConstructor p CoreTypes.nil []) reversedCaItems pushItem >> Ok
                    else
                        reversedFaRest
                        >> List.mapRes (fn hasDots & expr: translateRawPattern env expr) __
                        >> onOk fn reversedCaRest:
                        try lastFaExpr as

                            , FA.Lowercase { attrPath, maybeModule, maybeType, name }:
                                translatePatternAny env pos maybeType maybeModule name attrPath
                                >> onOk fn caInit:
                                List.for caInit reversedCaRest pushItem >> Ok

                            , _:
                                error env p [ "sorry, I don't understand the dots here..." ]

        , FA.Record { with  attrs, maybeExtension }:
            translatePatternRecord env pos maybeExtension attrs

        , FA.BinopChain precedence chain:
            if precedence == Op.precedence_tuple then
                chain
                >> translateTuple env.ro (translateRawPattern env __) __
                >> onOk fn recordAttrs:
                CA.PatternRecord pos CA.Complete recordAttrs >> Ok
            else if precedence == Op.precedence_cons then
                chain
                >> FA.binopChainExpressions
                >> List.mapRes (translateRawPattern env __) __
                >> onOk fn caPas:
                try List.reverse caPas as

                    , last :: rest:
                        last
                        >> List.for __ rest (fn item, list: CA.PatternConstructor pos CoreTypes.cons [ item, list ])
                        >> Ok

                    , []:
                        error env pos [ "should not happen: empty cons pattern" ]
            else
                error env pos [ "This binop can't be used in pattern matching" ]

        , FA.LiteralText singleOrTriple l:
            l
            >> escapeLiteralText singleOrTriple __
            >> CA.PatternLiteralText pos __
            >> Ok

        , FA.LiteralNumber isPercent l:
            translateNumber env.ro isPercent CA.PatternLiteralNumber pos l

        # Stuff that's not valid for patterns

        , FA.Uppercase _:
            error env pos [ "WUT" ]

        , FA.Statements stats:
            error env pos [ "WAT" ]

        , FA.Fn _ args body:
            error env pos [ "Can't pattern match on functions. =(" ]

        , FA.UnopCall unop expr:
            error env pos [ "This op can't be used in pattern matching" ]

        , FA.If _:
            error env pos [ "if..then can't be used in pattern matching" ]

        , FA.Try _:
            error env pos [ "try..as can't be used in pattern matching" ]


escapeLiteralText as fn Token.SingleOrTriple, Text: Text =
    fn singleOrTriple, l:
        try singleOrTriple as

            , Token.SingleQuote:
                l

            , Token.TripleQuote:
                l
                >> Text.replace "\"" "\\\"" __
                >> Text.replace "\n" "\\n" __


#
# Statement
#
translateStatements as fn Env, [ FA.Statement ]: Res CA.Expression =
    fn env, stats:
    try stats as

        , []:
            CoreTypes.noneValue
            >> CA.Constructor Pos.G __
            >> Ok

        , [ FA.Evaluation faExpression ]:
            translateExpression env faExpression

        , FA.CommentStatement _ :: tail:
            translateStatements env tail

        , FA.Evaluation faExpr :: tail:
            faExpr
            >> translateExpression env __
            >> onOk fn caExpr:
            caDef as CA.ValueDef =
                {
                , body = caExpr
                , directConsDeps = Dict.empty
                # TODO Do we need these here?
                , directTypeDeps =
                    Dict.empty
                , directValueDeps = Dict.empty
                , native = False
                , pattern = CA.PatternAny Pos.G Nothing Nothing
                , uni = Imm
                }

            tail
            >> translateStatements env __
            >> onOk fn acc:
            CA.LetIn caDef acc >> Ok

        , FA.ValueDef fa :: tail:
            fa
            >> translateDefinition False env __
            >> onOk fn newEnv & caDef:
            tail
            >> translateStatements newEnv __
            >> onOk fn acc:
            CA.LetIn caDef acc >> Ok

        , FA.AliasDef fa :: tail:
            error env fa.name.first [ "Aliases can be declared only in the root scope" ]

        , FA.UnionDef fa :: tail:
            error env fa.name.first [ "Types can be declared only in the root scope" ]


##
#- Expression
#
translateExpression as fn Env, FA.Expression: Res CA.Expression =
    fn env, FA.Expression _ pos expr_:
    try expr_ as

        , FA.LiteralNumber isPercent str:
            translateNumber env.ro isPercent CA.LiteralNumber pos str

        , FA.LiteralText singleOrTriple l:
            l
            >> escapeLiteralText singleOrTriple __
            >> CA.LiteralText pos __
            >> Ok

        , FA.Statements stats:
            translateStatements env stats

        , FA.Lowercase pas:
            translateLowercase env pos pas

        , FA.Uppercase _:
            error env pos [ "Can't reference a type or module here...?" ]

        , FA.Constructor { maybeModule, name }:
            resolveToConstructorUsr env.ro pos maybeModule name
            >> onOk fn usr:
            CA.Constructor pos usr >> Ok

        , FA.Fn _ faParams faBody:
            faParams
            >> List.mapRes (translateParameter env __) __
            >> onOk fn caParams:
            env
            >> List.forRes __ caParams fn par, envX:
                try par as
                    , CA.ParameterPattern uni pa: insertPatternNames False pa envX
                    , CA.ParameterRecycle p name: CA.PatternAny p (Just name) Nothing >> insertPatternNames False __ envX
                    , CA.ParameterPlaceholder n: { envX with values = Dict.insert (Text.fromNumber n) { isRoot = False, pos } .values } >> Ok
            >> onOk fn localEnv:
            faBody
            >> translateExpression localEnv __
            >> onOk fn caBody:
            CA.Fn pos caParams caBody >> Ok

        [# TODO
        , FA.Call (FA.Expression p (FA.Fn faPars faBody)) faArgs:

            - check that args and pairs are the same length

            - for any arg
                if non-trivial
                    and
                    contains a recyclable or is used in faBody more than once (watch out for shadowing!!!)
                then
                    set a let-in
                else
                    use as-is

            - inline the args inside faBody
        #]

        , FA.ResolvedArgumentPlaceholder n:
            CA.Variable pos (RefPlaceholder n) >> Ok

        , FA.Call faRef faArgs:
            placeholdersCount & reversedArgs =
                List.for (0 & []) faArgs fn exp, cnt & rev:
                    if isPlaceholder exp then
                        FA.Expression c p _ =
                            exp

                        cnt + 1 & [ FA.Expression c p (FA.ResolvedArgumentPlaceholder cnt), ...rev ]
                    else
                        cnt & [ exp, ...rev ]

            if placeholdersCount > 0 then
                FA.Call faRef (List.reverse reversedArgs) >> makePartiallyAppliedFunction env pos placeholdersCount __
            else
                faRef
                >> translateExpression env __
                >> onOk fn caRef:
                faArgs
                >> List.mapRes (translateArgument env __) __
                >> onOk fn caArgs:
                CA.Call pos caRef caArgs >> Ok

        , FA.If { with  condition, false, true }:
            translateExpression env condition
            >> onOk fn c:
            translateExpression env true
            >> onOk fn t:
            translateExpression env false
            >> onOk fn f:
            {
            , condition = c
            , false = f
            , true = t
            }
            >> CA.If pos __
            >> Ok

        , FA.UnopCall opId faOperand:
            try opId as

                , Op.UnopUnique:
                    error env pos [ "can't use ! here because REASONS" ]

                , Op.UnopRecycle:
                    error env pos [ "can recycle only in function calls!" ]

                , Op.UnopPlus:
                    translateExpression env faOperand

                , Op.UnopMinus:
                    faOperand
                    >> translateExpression env __
                    >> onOk fn caOperand:
                    CA.Call pos (CA.Variable pos (RefGlobal Prelude.unaryMinus.usr)) [ CA.ArgumentExpression caOperand ] >> Ok

        , FA.BinopChain group chain:
            translateBinopChain env pos group chain

        , FA.Record { with  attrs, maybeExtension }:
            translateRecord env pos maybeExtension attrs

        , FA.List _ faDotsAndItems:
            rev =
                List.reverse faDotsAndItems

            try rev as

                , []:
                    CA.Constructor pos CoreTypes.nil >> Ok

                , hasDots & head :: rest:
                    if List.any Tuple.first rest then
                        error env pos [ "can use dots only on the last element (for now?)" ]
                    else
                        init & revItems =
                            if hasDots then
                                head & rest
                            else
                                FA.Expression [] pos (FA.List False []) & rev

                        translateExpression env init
                        >> onOk fn caInit:
                        caInit
                        >> List.forRes __ revItems fn _ & faItem, acc:
                            translateExpression env faItem
                            >> onOk fn caItem:
                            CA.Call pos (CA.Constructor pos CoreTypes.cons) [ CA.ArgumentExpression caItem, CA.ArgumentExpression acc ] >> Ok

        , FA.Try { patterns, value }:
            if isPlaceholder value then
                FA.Try { patterns, value = FA.Expression [] pos (FA.ResolvedArgumentPlaceholder 0) } >> makePartiallyAppliedFunction env pos 1 __
            else
                translatePatternAndStatements as fn FA.Expression & FA.Expression: Res (Uniqueness & CA.Pattern & CA.Expression) =
                    fn faPattern & faExpression:
                    faPattern
                    >> translateFullPattern env __
                    >> onOk fn uni & caPattern:
                    env
                    >> insertPatternNames False caPattern __
                    >> onOk fn localEnv:
                    faExpression
                    >> translateExpression localEnv __
                    >> onOk fn block:
                    uni & caPattern & block >> Ok

                translateExpression env value
                >> onOk fn caValue:
                patterns
                >> List.mapRes translatePatternAndStatements __
                >> onOk fn patternsAndExpressions:
                CA.Try pos { patternsAndExpressions, value = caValue } >> Ok

        , _:
            error env pos [ "something's wrong here...", toHuman expr_ ]


makePartiallyAppliedFunction as fn Env, Pos, Int, FA.Expr_: Res CA.Expression =
    fn env, pos, placeholdersCount, body:
    ex =
        FA.Expression [] pos __

    List.range 0 (placeholdersCount - 1)
    >> List.map (fn x: x >> FA.ResolvedArgumentPlaceholder >> ex) __
    >> FA.Fn FA.Inline __ (ex body)
    >> ex
    >> translateExpression env __


insertPatternNames as fn Bool, CA.Pattern, Env: Res Env =
    fn isRoot, pattern, env:
    List.forRes env.values (CA.patternNames pattern) fn paName, vs:
        try Dict.get paName.name vs as

            , Just duplicateName:
                error
                    env
                    paName.pos
                    [
                    , "A variable named `" .. paName.name .. "` has already been defined."
                    # TODO display earlier location
                    , "You need to find a less ambiguous name."
                    ]

            , Nothing:
                isUnique =
                    try Dict.get paName.name env.ro.meta.globalValues as
                        , Nothing: True
                        , Just globalUsr: isRoot and globalUsr == USR env.ro.umr paName.name

                if isUnique then
                    Dict.insert paName.name { isRoot, pos = paName.pos } vs >> Ok
                else
                    error
                        env
                        paName.pos
                        [
                        , "There is already a global variable named `" .. paName.name .. "`."
                        , "You need to find a different name, or modify modules.sp"
                        ]
    >> onOk fn values:
    Ok { env with values }


translateLowercase as fn Env, Pos, { attrPath as [ Name ], maybeModule as Maybe Name, maybeType as Maybe FA.Expression, name as Name }: Res CA.Expression =
    fn env, pos, { attrPath, maybeModule, maybeType, name }:
    if maybeType /= Nothing then
        error env pos [ "no annotations on var reference" ]
    else
        isRoot =
            try Dict.get name env.values as
                , Nothing: True
                , Just paName: paName.isRoot

        resolveToValueRef env.ro pos isRoot maybeModule name
        >> onOk fn usr:
        CA.Variable pos usr
        >> List.for __ attrPath (CA.RecordAccess pos __ __)
        >> Ok


translateRecordShorthand as fn Env, Pos, [ Name ], Name: Res CA.Expression =
    fn env, pos, attrPath, name:
        try env.maybeShorthandTarget as

            , Nothing:
                error
                    env
                    pos
                    [
                    , "Record update shorthands must be used inside a record update such as"
                    , "    { aRecord with anAttribute = doSomethingWith ." .. Text.join "." attrPath .. " }"
                    , "but we are not inside a record update!"
                    ]

            , Just shorthandTarget:
                shorthandTarget
                >> List.for __ (name :: attrPath) (fn attrName, expr: CA.RecordAccess pos attrName expr)
                >> Ok


translateParameter as fn Env, FA.Expression: Res CA.Parameter =
    fn env, fa:
    FA.Expression _ pos faExpr =
        fa

    try faExpr as

        , FA.UnopCall Op.UnopRecycle (FA.Expression _ p faOperand):
            try faOperand as

                , FA.Lowercase { attrPath, maybeModule, maybeType = Nothing, name }:
                    if maybeModule /= Nothing or attrPath /= [] then
                        error env pos [ "I was expecting a local variable name here... =|" ]
                    else
                        CA.ParameterRecycle pos name >> Ok

                , _:
                    error env p [ "@ should be followed by a variable name to recycle!" ]

        , FA.ResolvedArgumentPlaceholder n:
            CA.ParameterPlaceholder n >> Ok

        , _:
            translateFullPattern env fa
            >> onOk fn uni & ca:
            CA.ParameterPattern uni ca >> Ok


translateNumber as fn ReadOnly, Bool, fn Pos, Number: a, Pos, Text: Res a =
    fn ro, isPercent, constructor, pos, numberAsText:
    try Text.toNumber (Text.replace "_" "" numberAsText) as

        , Nothing:
            erroro
                ro
                pos
                [
                , "invalid number: `" .. numberAsText .. "`"
                , "TODO link to documentation on valid number formats"
                ]

        , Just n:
            Ok << constructor pos (if isPercent then n / 100 else n)


translateRecord as fn Env, Pos, Maybe (Maybe FA.Expression), [ FA.RecordAttribute ]: Res CA.Expression =
    fn env, pos, maybeMaybeExtension, attrs:
    zzz as Res (Maybe CA.Expression) =
        try maybeMaybeExtension as
            , Just (Just ext): translateExpression env ext >> Result.map Just __
            , Just Nothing: error env pos [ "I need to know what record you are updating" ]
            , Nothing: Ok Nothing

    zzz
    >> onOk fn maybeCaExt:
    try maybeCaExt as

        , Nothing:
            Dict.empty
            >> List.forRes __ attrs (translateAndInsertRecordAttribute { env with maybeShorthandTarget = Nothing } __ __)
            >> onOk fn caAttrs:
            CA.Record pos Nothing caAttrs >> Ok

        , Just caExt:
            varName =
                Text.fromNumber env.nextGeneratedVariableName

            var =
                CA.Variable Pos.G (RefLocal varName)

            newEnv =
                { env with
                , maybeShorthandTarget = Just var
                , nextGeneratedVariableName = .nextGeneratedVariableName + 1
                }

            Dict.empty
            >> List.forRes __ attrs (translateAndInsertRecordAttribute newEnv __ __)
            >> onOk fn caAttrs:
            def as CA.ValueDef =
                {
                , body = caExt
                , directConsDeps = Dict.empty
                # TODO populate deps?
                , directTypeDeps =
                    Dict.empty
                , directValueDeps = Dict.empty
                , native = False
                , pattern = CA.PatternAny Pos.G (Just varName) Nothing
                # TODO ----> This desugaring needs to be done by TypeCheck, because MakeCanonical can't infer its uniqueness
                , uni =
                    Imm
                }

            caAttrs
            >> CA.Record pos (Just var) __
            >> CA.LetIn def __
            >> Ok


translateAndInsertRecordAttribute as fn Env, FA.RecordAttribute, Dict Text CA.Expression: Res (Dict Text CA.Expression) =
    fn env, attr, caAttrsAccum:
    translateAttributeName env.ro attr.name
    >> onOk fn pos & caName & maybeFaType:
    if Dict.member caName caAttrsAccum then
        error env pos [ "duplicate attribute: " .. caName ]
    else
        attr.maybeExpr
        >> Maybe.withDefault attr.name __
        >> translateExpression env __
        >> onOk fn caExpr:
        caAttrsAccum
        >> Dict.insert caName caExpr __
        >> Ok


translateArgument as fn Env, FA.Expression: Res CA.Argument =
    fn env, faExpression:
    FA.Expression _ pos expr =
        faExpression

    try expr as

        , FA.UnopCall Op.UnopRecycle (FA.Expression _ _ faOperand):
            try faOperand as

                , FA.Lowercase { attrPath, maybeModule, maybeType, name }:
                    if maybeType /= Nothing then
                        error env pos [ "Sorry, at least for now annotations are not supported here" ]
                    else if maybeModule /= Nothing then
                        error env pos [ "Only values declared inside a function scope can be mutated!" ]
                    else
                        CA.ArgumentRecycle pos name attrPath >> Ok

                , _:
                    error env pos [ "I can recycle only variables!" ]

        , FA.ArgumentPlaceholder:
            error env pos [ "compiler error: this should have been eliminated already" ]

        , FA.ResolvedArgumentPlaceholder n:
            CA.ArgumentExpression (CA.Variable pos (RefPlaceholder n)) >> Ok

        , _:
            faExpression
            >> translateExpression env __
            >> onOk fn caExpr:
            CA.ArgumentExpression caExpr >> Ok


isPlaceholder as fn FA.Expression: Bool =
    fn FA.Expression _ _ expr:
    try expr as
        , FA.ArgumentPlaceholder: True
        , _: False


translateBinopChain as fn Env, Pos, Int, FA.BinopChain: Res CA.Expression =
    fn env, pos, group, opChain:
    toExpression as fn FA.Expr_: FA.Expression =
        FA.Expression [] pos __

    cnt0 & head =
        if isPlaceholder opChain.first then
            1 & toExpression (FA.ResolvedArgumentPlaceholder 0)
        else
            0 & opChain.first

    (placeholdersCount as Int) & (reversedChainTail as [ FA.Binop & FA.Expression ]) =
        List.for (cnt0 & []) opChain.second fn op & exp, cnt & rev:
            if isPlaceholder exp then
                FA.Expression c p _ =
                    exp

                cnt + 1 & [ op & FA.Expression c p (FA.ResolvedArgumentPlaceholder cnt), ...rev ]
            else
                cnt & [ op & exp, ...rev ]

    if placeholdersCount > 0 then
        FA.BinopChain group (head & List.reverse reversedChainTail) >> makePartiallyAppliedFunction env pos placeholdersCount __
    else if group == Op.precedence_pipe then
        resolvePipe env pos opChain >> onOk (translateExpression env __)
    else if group == Op.precedence_tuple then
        translateTupleExpression env pos opChain
    else if group == Op.precedence_comparison then
        translateComparison env pos opChain
    else if group == Op.precedence_logical then
        translateLogical env pos opChain
    else if group == Op.precedence_mutop then
        translateMutop env pos opChain
    else
        translateRightAssociativeBinopChain env pos opChain


resolvePipe as fn Env, Pos, FA.BinopChain: Res FA.Expression =
    fn env, pos, opChain:
    if FA.binopChainAllBinops (fn sep: sep.usr == Prelude.sendRight.usr) opChain then
        #
        #   head >> b >> c >> d    --->   d (c (b head))
        #
        head & chainTail =
            opChain

        List.for head chainTail fn sep & faExp, acc:
            FA.Expression _ p _ =
                faExp

            FA.Expression [] p (FA.Call faExp [ acc ])
        >> Ok
    else if FA.binopChainAllBinops (fn sep: sep.usr == Prelude.sendLeft.usr) opChain then
        #
        #   head << b << c << d    --->   head (b (c d))
        #
        last & body =
            FA.binopChainReverse opChain

        List.for last body fn sep & faExp, acc:
            FA.Expression _ p _ =
                faExp

            FA.Expression [] p (FA.Call faExp [ acc ])
        >> Ok
    else
        error env pos [ "Mixing `>>` and `<<` is ambiguous. Use parens!" ]


translateTupleExpression as fn Env, Pos, FA.BinopChain: Res CA.Expression =
    fn env, pos, one & chainTail:
    try chainTail as

        , []:
            translateExpression env one

        , [ _ & two ]:
            translateExpression env one
            >> onOk fn first:
            translateExpression env two
            >> onOk fn second:
            Dict.empty
            >> Dict.insert "first" first __
            >> Dict.insert "second" second __
            >> CA.Record pos Nothing __
            >> Ok

        , [ _ & two, _ & three ]:
            translateExpression env one
            >> onOk fn first:
            translateExpression env two
            >> onOk fn second:
            translateExpression env three
            >> onOk fn third:
            Dict.empty
            >> Dict.insert "first" first __
            >> Dict.insert "second" second __
            >> Dict.insert "third" third __
            >> CA.Record pos Nothing __
            >> Ok

        , _:
            error env pos [ "Tuples can't have more than 3 items, use a record instead." ]


translateComparison as fn Env, Pos, FA.BinopChain: Res CA.Expression =
    fn env, pos, opChain:
    try opChain.second as

        , []:
            translateExpression env opChain.first

        , [ sep & second ]:
            translateRightAssociativeBinopChain env pos opChain

        , [ firstSep & second, ...moar ]:
            if FA.binopChainAllBinops (sameDirectionAs firstSep __) opChain then
                # TODO expand `a < b < c` to `a < b and b < c` without calculating b twice
                error env pos [ "TODO: not (yet) implemented: compops expansion" ]
            else
                # TODO actually list the seps
                error env pos [ "can't mix comparison ops with different direction" ]


translateLogical as fn Env, Pos, FA.BinopChain: Res CA.Expression =
    fn env, pos, opChain:
    allSame =
        FA.binopChainAllBinops (fn sep: sep.usr == Prelude.and_.usr) opChain or FA.binopChainAllBinops (fn sep: sep.usr == Prelude.or_.usr) opChain

    if allSame then
        translateRightAssociativeBinopChain env pos opChain
    else
        error env pos [ "Mixing `and` and `or` is ambiguous. Use parens!" ]


translateMutop as fn Env, Pos, FA.BinopChain: Res CA.Expression =
    fn env, pos, left & chainTail:
    try chainTail as

        , []:
            translateExpression env left

        , [ op & right ]:
            caRef =
                CA.Variable op.pos (RefGlobal op.usr)

            [ left, right ]
            >> List.mapRes (translateArgument env __) __
            >> onOk fn caArgs:
            CA.Call pos caRef caArgs >> Ok

        , _:
            error env pos [ "mutops can't be chained" ]


sameDirectionAs as fn FA.Binop, FA.Binop: Bool =
    fn a, b:
    if a.symbol == b.symbol then
        True
    else
        try a.symbol as
            , ">": b.symbol == ">="
            , ">=": b.symbol == ">"
            , "<": b.symbol == "<="
            , "<=": b.symbol == "<"
            , _: False


translateRightAssociativeBinopChain as fn Env, Pos, FA.BinopChain: Res CA.Expression =
    fn env, pos, faLeft & faOpsAndRight:
    try faOpsAndRight as

        , []:
            translateExpression env faLeft

        , [ op & faRight, ...faTail ]:
            translateArgument env faLeft
            >> onOk fn caLeft:
            translateRightAssociativeBinopChain env pos (faRight & faTail)
            >> onOk fn caRight:
            caRef =
                CA.Variable op.pos (RefGlobal op.usr)

            CA.Call pos caRef [ caLeft, CA.ArgumentExpression caRight ] >> Ok


#
# Type
#

translateAndInsertRecordAttributeType as fn ReadOnly, FA.RecordAttribute, Dict Name CA.RawType: Res (Dict Name CA.RawType) =
    fn ro, faAttr, caAttrs:
    translateAttributeName ro faAttr.name
    >> onOk fn pos & name & maybeFaType:
    if Dict.member name caAttrs then
        erroro ro pos [ "Duplicate attribute name: " .. name ]
    else
        try maybeFaType as

            , Nothing:
                erroro ro pos [ "I need to see the type of this attribute, `" .. name .. " as TheType`" ]

            , Just faType:
                faType
                >> translateRawType ro __
                >> onOk fn caType:
                if faAttr.maybeExpr /= Nothing then
                    erroro ro pos [ "I'm expecting a type here; `=` is for assignign values" ]
                else
                    caAttrs
                    >> Dict.insert name caType __
                    >> Ok


translateTypeFunctionParameter as fn ReadOnly, FA.Expression: Res CA.ParType =
    fn ro, expression:
    FA.Expression _ _ expr_ =
        expression

    try expr_ as

        , FA.UnopCall Op.UnopRecycle faOperand:
            faOperand
            >> translateRawType ro __
            >> Result.map CA.ParRe __

        , _:
            expression
            >> translateFullType ro __
            >> Result.map CA.ParSp __


translatePoly as fn ReadOnly, FA.Expression: Res (Uniqueness & FA.Expression) =
    fn ro, expr:
    FA.Expression _ pos expr_ =
        expr

    try expr_ as

        , FA.UnopCall Op.UnopUnique e:
            Uni & e >> Ok

        , FA.Poly numberAsString e:
            try Text.toNumber numberAsString as
                , Nothing: erroro ro pos [ "I need an integer number here" ]
                , Just n: Depends n & e >> Ok

        , _:
            Imm & expr >> Ok


translateFullType as fn ReadOnly, FA.Expression: Res CA.FullType =
    fn ro, expr:
    expr
    >> translatePoly ro __
    >> onOk fn uni & e:
    translateRawType ro e
    >> onOk fn raw:
    { raw, uni } >> Ok


translateRawType as fn ReadOnly, FA.Expression: Res CA.RawType =
    fn ro, FA.Expression _ pos expr_:
    try expr_ as

        , FA.Uppercase { maybeModule, name }:
            resolveToTypeUsr ro pos maybeModule name
            >> onOk fn usr:
            CA.TypeNamed pos usr [] >> Ok

        , FA.Lowercase { attrPath, maybeModule, maybeType, name }:
            if maybeType /= Nothing then
                erroro ro pos [ "Can't really specify the type of a type." ]
            else if maybeModule /= Nothing then
                erroro ro pos [ "no modules for tyvars!" ]
            else if attrPath /= [] then
                erroro ro pos [ "no attributes for tyvars!" ]
            else
                CA.TypeAnnotationVariable pos name >> Ok

        , FA.Call (FA.Expression _ refPos ref) faArgs:
            try ref as

                , FA.Uppercase { maybeModule, name }:
                    faArgs
                    >> List.mapRes (translateRawType ro __) __
                    >> onOk fn caArgs:
                    resolveToTypeUsr ro pos maybeModule name
                    >> onOk fn usr:
                    CA.TypeNamed pos usr caArgs >> Ok

                , _:
                    erroro ro refPos [ "I was expecting a named type here" ]

        , FA.List _ dotsAndItems:
            try dotsAndItems as

                , []:
                    erroro ro pos [ "You need to specify the type of the List items" ]

                , [ hasDots & faItem ]:
                    if hasDots then
                        erroro ro pos [ "No need to use dots here" ]
                    else
                        translateRawType ro faItem
                        >> onOk fn caItem:
                        CoreTypes.list caItem >> Ok

                , _:
                    erroro ro pos [ "List items must all have the same type, so you can specify only one type" ]

        , FA.Record { with  attrs, maybeExtension }:
            if maybeExtension /= Nothing then
                erroro ro pos [ "Experimentally, extensible type annotations are disabled" ]
            else
                Dict.empty
                >> List.forRes __ attrs (translateAndInsertRecordAttributeType ro __ __)
                >> onOk fn caAttrs:
                caAttrs
                >> CA.TypeRecord pos __
                >> Ok

        , FA.Fn _ faParams faReturn:
            faParams
            >> List.mapRes (translateTypeFunctionParameter ro __) __
            >> onOk fn caParams:
            faReturn
            >> translateFullType ro __
            >> onOk fn caReturn:
            CA.TypeFn pos caParams caReturn >> Ok

        , FA.BinopChain precedence chain:
            if precedence == Op.precedence_tuple then
                chain
                >> translateTuple ro (translateRawType ro __) __
                >> onOk fn recordAttrs:
                CA.TypeRecord pos recordAttrs >> Ok
            else
                erroro ro pos [ "This operator can't be used in type definitions", toHuman expr_ ]

        , _:
            # TODO: do all other constructors explicitly
            erroro ro pos [ "Not sure what's up with this type =|", toHuman expr_ ]


#
# Union constructor
#

translateConstructor as fn CA.RawType, USR, FA.Expression, Dict Name CA.Constructor & Env: Res (Dict Name CA.Constructor & Env) =
    fn unionType, unionUsr, FA.Expression _ pos expr_, constructors & env:
    try expr_ as

        , FA.Constructor { maybeModule = Nothing, name }:
            name & [] >> Ok

        , FA.Call (FA.Expression _ _ (FA.Constructor { maybeModule = Nothing, name })) pars:
            name & pars >> Ok

        , _:
            error
                env
                pos
                [
                , "I was expecting a 'constructor name here!"
                ]
    >> onOk fn name & faPars:
    if Dict.member name constructors then
        # TODO "union $whatever has two constructors with the same name!"
        error env pos [ "constructor " .. name .. " is duplicate" ]
    else
        faPars
        >> List.mapRes (translateRawType env.ro __) __
        >> onOk fn ins:
        env
        >> insertPatternNames True (CA.PatternAny pos (Just name) Nothing) __
        >> onOk fn newEnv:
        c as CA.Constructor =
            {
            , ins
            , out = unionType
            , pos
            , typeUsr = unionUsr
            }

        Dict.insert name c constructors & newEnv >> Ok


#
# Module
#

insertRootStatement as fn FA.Statement, CA.Module & Env: Res (CA.Module & Env) =
    fn faStatement, caModule & env:
    try faStatement as

        , FA.Evaluation (FA.Expression _ pos _):
            error env pos [ "Root Evaluations don't really do much =|" ]

        , FA.ValueDef d:
            d
            >> translateDefinition True env __
            >> onOk fn newEnv & def:
            if def.uni /= Imm then
                error env (CA.patternPos def.pattern) [ "Unique values can be declared only inside functions." ]
            else
                # Patterns contain position, so they are unique and don't need to be checked for duplication
                { caModule with valueDefs = Dict.insert def.pattern def .valueDefs } & newEnv >> Ok

        , FA.AliasDef fa:
            pos & name =
                fa.name

            if Dict.member name caModule.aliasDefs or Dict.member name caModule.unionDefs then
                error env fa.name.first [ name .. " declared twice!" ]
            else
                # TODO check that args are not duplicate

                fa.type
                >> translateRawType env.ro __
                >> onOk fn type:
                aliasDef as CA.AliasDef =
                    {
                    , directTypeDeps = typeDeps type Set.empty
                    , pars = List.map (fn p & n: n & p) fa.args
                    , type
                    , usr = USR env.ro.umr name
                    }

                { caModule with aliasDefs = Dict.insert name aliasDef .aliasDefs } & env >> Ok

        , FA.UnionDef fa:
            pos & name =
                fa.name

            if Dict.member name caModule.aliasDefs or Dict.member name caModule.unionDefs then
                error env pos [ name .. " declared twice!" ]
            else
                # TODO check that args are not duplicate
                caPars =
                    List.map (fn p & n: n & p) fa.args

                usr =
                    USR env.ro.umr name

                type =
                    caPars
                    >> List.map (fn n & p: CA.TypeAnnotationVariable p n) __
                    >> CA.TypeNamed pos usr __

                Dict.empty & env
                >> List.forRes __ fa.constructors (translateConstructor type usr __ __)
                >> onOk fn constructors & newEnv:
                unionDef as CA.UnionDef =
                    {
                    , constructors
                    # I could probably break the deps by constructor, but would it be much useful in practice?
                    , directTypeDeps =
                        Dict.for Set.empty constructors (fn k, c, z: List.for z c.ins typeDeps)
                    , pars = caPars
                    , usr
                    }

                { caModule with unionDefs = Dict.insert name unionDef .unionDefs } & newEnv >> Ok


translateModule as fn ReadOnly, FA.Module: Res CA.Module =
    fn ro, faModule:
    Debug.benchStart None

    module =
        CA.initModule ro.errorModule.fsPath ro.umr ro.errorModule.content

    # Add all definitions
    module & initEnv ro
    >> List.forRes __ faModule (insertRootStatement __ __)
    >> Result.map Tuple.first __
    >> btw Debug.benchStop "translateModule" __


textToCanonicalModule as fn Bool, ReadOnly: Res CA.Module =
    fn stripLocations, ro:
    {
    , errorModule = ro.errorModule
    , keepComments = False
    , stripLocations
    }
    >> Compiler/Parser.textToFormattableModule
    >> Result.onOk fn faModule:
    translateModule ro faModule
