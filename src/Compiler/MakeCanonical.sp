
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
    #
    # Non-root values don't need to be expanded with the module name.
    #
    , values as Dict Name { pos as Pos, isRoot as Bool }
    #
    # TODO This is used to tell the user that definitions must be in order
    #
    #, futureNonRootValues as Dict Text Pos
    #
    #
    , ro as ReadOnly

    , nonFn as Dict Name Pos
    }


alias ReadOnly =
    {
    , errorModule as Error.Module
    , umr as UMR
    , meta as Meta
    }


initEnv as fn ReadOnly: Env =
    fn ro:
    {
    , maybeShorthandTarget = Nothing
    , nextGeneratedVariableName = 0
    , values = Dict.empty
    #, futureNonRootValues = Dict.empty
    , ro = ro
    , nonFn = Dict.empty
    }


#
# Errors
#


erroro as fn ReadOnly, Pos, [Text]: Res a =
    fn ro, pos, msg:
    Error.res ro.errorModule pos msg

error as fn Env, Pos, [Text]: Res a =
    fn env, pos, msg:
    Error.res env.ro.errorModule pos msg


#
# Names resolution
#

maybeForeignUsr as fn (fn Meta: Dict Text USR), ReadOnly, Pos, Maybe Name, Name: Res (Maybe USR) =
    fn getter, ro, pos, maybeModule, name:

    try maybeModule as
        , Nothing:
            Dict.get name (getter ro.meta)
            >> Ok

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



resolveToUsr as fn (fn Meta: Dict Text USR), ReadOnly, Pos, Maybe Name, Name: Res USR =
    fn getter, ro, pos, maybeModule, name:

    maybeForeignUsr getter ro pos maybeModule name
    >> Result.map (Maybe.withDefault (USR ro.umr name) __) __


resolveToValueRef as fn ReadOnly, Pos, Bool, Maybe Name, Name: Res Ref =
    fn ro, pos, isRoot, maybeModule, name:

    # TODO use Result.map?
    try maybeForeignUsr (fn m: m.globalValues) ro pos maybeModule name as
        , Err e:
            Err e

        , Ok (Just usr):
            RefGlobal usr
            >> Ok

        , Ok Nothing:
            if isRoot then
                USR ro.umr name
                >> RefGlobal
                >> Ok

            else
                RefLocal name
                >> Ok


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
        , CA.TypeNamed _ usr args: acc >> Set.insert usr __ >> List.for __ args typeDeps
        , CA.TypeAnnotationVariable _ _: acc
        , CA.TypeRecord _ attrs: Dict.for acc attrs (fn k, v, a: typeDeps v a)
        , CA.TypeError _: acc
        , CA.TypeFn _ params to:
            acc
            >> typeDeps to.raw __
            >> List.for __ params fn par, z:
                try par as
                  , CA.ParRe raw: typeDeps raw z
                  , CA.ParSp full: typeDeps full.raw z


alias Deps =
    {
    , types as Set USR
    , cons as Set USR
    , values as Set USR
    }


deps_init =
    {
    , types = Set.empty
    , cons = Set.empty
    , values = Set.empty
    }


patternDeps as fn CA.Pattern, Deps: Deps =
    fn pattern, deps:

    try pattern as

        , CA.PatternConstructor _ usr ps:
            List.for { deps with cons = Set.insert usr .cons } ps patternDeps

        , CA.PatternRecord _ completeness ps:
            Dict.for deps ps (fn k, v, a: patternDeps v a)

        , CA.PatternAny _ _ (Just annotation):
            { deps with types = typeDeps annotation.raw .types }

        , CA.PatternAny _ _ Nothing:
            deps

        , CA.PatternLiteralNumber _ _:
           deps

        , CA.PatternLiteralText _ _:
           deps


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

        , CA.Try _ { value, patternsAndExpressions }:
            deps
            >> expressionDeps value __
            >> List.for __ patternsAndExpressions (fn (u & p & b), d: d >> patternDeps p __ >> expressionDeps b __)

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

    fa.nonFn
    >> List.mapRes (translateTypeParameter env.ro __) __
    >> onOk fn nonFn:

    fa.pattern
    >> translateFullPattern { env with nonFn = Dict.fromList nonFn } __
    >> onOk fn (uni & pattern):

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
    &
    {
    , uni
    , pattern
    , native = False
    , body
    #
    , directTypeDeps = deps.types
    , directConsDeps = deps.cons
    , directValueDeps = deps.values
    }
    >> Ok




#
# Pattern
#
translateAttributeName as fn ReadOnly, FA.Expression: Res (Pos & Name & Maybe FA.Expression) =
    fn ro, (FA.Expression pos expr_):

    try expr_ as
        , FA.Variable { maybeType, word }:

            if word.modifier /= Token.NameNoModifier then
                erroro ro pos [ "attribute names can't start with a dot" ]
            else if word.isUpper then
                erroro ro pos [ "attribute names must be lower case" ]
            else if word.maybeModule /= Nothing then
                erroro ro pos [ "attribute names must be single words" ]
            else if word.attrPath /= [] then
                erroro ro pos [ "attribute names can't contain dots" ]
            else
                pos & word.name & maybeType >> Ok

        , _:
            erroro ro pos [ "Expecting an attribute name here" ]


translatePatternConstructor as fn Env, Pos, Token.Word, [CA.Pattern]: Res CA.Pattern =
    fn env, pos, word, args:

    if word.modifier /= Token.NameNoModifier then
        error env pos [ "Constructor names cannot have modifiers" ]
    else if word.attrPath /= [] then
        error env pos [ "Constructors don't have attributes" ]
    else
        resolveToConstructorUsr env.ro pos word.maybeModule word.name
        >> onOk fn usr:

        CA.PatternConstructor pos usr args
        >> Ok


translatePatternAny as fn Env, Pos, Maybe FA.Expression, Token.Word: Res CA.Pattern =
    fn env, pos, maybeFaType, word:

    if word.modifier /= Token.NameNoModifier then
        error env pos [ "Record access shorthands" ]
    else if word.attrPath /= [] then
        error env pos [ "To access attributes in pattern matching use { with theAttributeName = theVariableName }" ]
    else if word.maybeModule /= Nothing then
        error env pos [ "You can't access modules here..." ]
    else
        translateMaybeAnnotation env maybeFaType
        >> onOk fn maybeAnnotation:

        maybeName =
            if word.name == "_" then Nothing else Just word.name

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
                CA.typeTyvars raw
                >> Dict.map (fn tyvarName, pos: { nonFn = Dict.get tyvarName env.nonFn }) __

            { raw, tyvars, univars = CA.typeUnivars raw }
            >> Just
            >> Ok


insertPatternRecordAttribute as fn Env, FA.RecordAttribute, Dict Name CA.Pattern: Res (Dict Name CA.Pattern) =
    fn env, attr, caAttrs:

    # { x }
    # { x = pattern }
    # { x as Type }

    translateAttributeName env.ro attr.name
    >> onOk fn (pos & caName & maybeFaType):

    if Dict.member caName caAttrs then
        error env pos [ "duplicate attribute name in pattern: " .. caName ]

    else
        try attr.maybeExpr & maybeFaType as
            , Just _ & Just (FA.Expression typePos _):
                error env typePos [ "if you want to annotate the attribute, use { x = y as TheType }" ]

            , Nothing & Just faType:
                error env pos [ "TODO annotating record attributes needs more thinking" ]
#                translateRawType env.ro faType
#                >> onOk fn caType:
#
#                caAttrs
#                >> Dict.insert caName (CA.PatternAny pos { maybeName = Just caName, maybeAnnotation = Just caType }) __
#                >> Ok

            , Just faPattern & Nothing:
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


translatePatternRecord as fn Env, Pos, Maybe (Maybe FA.Expression), [{ name as FA.Expression, maybeExpr as Maybe FA.Expression }]: Res CA.Pattern =
    fn env, pos, maybeMaybeExt, attrs:

    zzz =
        try maybeMaybeExt as
            , Just (Just (FA.Expression p expr_)):
                error env p [ "Can't extend patterns" ]

            , Just Nothing:
                # { with attr1 = ... }
                Ok CA.Partial

            , Nothing:
                # { attr1 = ... }
                Ok CA.Complete

    zzz
    >> onOk fn completeness:

    Dict.empty
    >> List.forRes __ attrs (insertPatternRecordAttribute env __ __)
    >> Result.map (fn x: CA.PatternRecord pos completeness x) __


translateTuple as fn ReadOnly, (fn FA.Expression: Res ca), FA.SepList Op.Binop FA.Expression: Res (Dict Name ca) =
    fn ro, translate, sepList:

    faExpressions as [FA.Expression] =
        FA.sepToList sepList

    faExpressions
    >> List.mapRes translate __
    >> onOk fn items:

    pos as Pos =
        List.for Pos.G faExpressions (fn (FA.Expression p _), z: Pos.range p z)

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
    >> onOk fn (uni & e):

    translateRawPattern env e
    >> onOk fn caPa:

    uni & caPa
    >> Ok


translateRawPattern as fn Env, FA.Expression: Res CA.Pattern =
    fn env, (FA.Expression pos expr_):

    try expr_ as

        , FA.Variable { maybeType, word }:

            if word.isUpper then
                if maybeType /= Nothing then
                    error env pos [ "Pattern constructors can't have type annotations" ]
                else
                    translatePatternConstructor env pos word []
            else
                translatePatternAny env pos maybeType word

        , FA.Call (FA.Expression p ref) faArgs:
            try ref as
                , FA.Variable { maybeType, word }:
                    if not word.isUpper then
                        error env p [ "I need an uppercase constructor name here" ]
                    else if maybeType /= Nothing then
                        error env p [ "Constructors can't be annotated (yet? would it make sense?)" ]
                    else
                        faArgs
                        >> List.mapRes (translateRawPattern env __) __
                        >> onOk fn caPars:
                        translatePatternConstructor env p word caPars

                , _:
                    error env p [ "I was expecting a constructor name here" ]

        , FA.List faItems:

            reversedFaItems =
                List.reverse faItems

            pushItem as fn CA.Pattern, CA.Pattern: CA.Pattern =
                fn pattern, last:
                CA.PatternConstructor (CA.patternPos pattern) CoreTypes.cons [ pattern, last ]

            try reversedFaItems as
                , []:
                    CA.PatternConstructor pos CoreTypes.nil []
                    >> Ok

                , [lastHasDots & FA.Expression p lastFaExpr, ...reversedFaRest]:
                    if List.any Tuple.first reversedFaRest then
                        error env p [ "only the last item in a list can have ... triple dots" ]

                    else if not lastHasDots then
                        reversedFaItems
                        >> List.mapRes (fn (hasDots & expr): translateRawPattern env expr) __
                        >> onOk fn reversedCaItems:

                        List.for (CA.PatternConstructor p CoreTypes.nil []) reversedCaItems pushItem
                        >> Ok

                    else
                        reversedFaRest
                        >> List.mapRes (fn (hasDots & expr): translateRawPattern env expr) __
                        >> onOk fn reversedCaRest:

                        try lastFaExpr as
                            , FA.Variable { maybeType, word }:
                                translatePatternAny env p maybeType word
                                >> onOk fn caInit:

                                List.for caInit reversedCaRest pushItem
                                >> Ok

                            , _:
                                error env p [ "sorry, I don't understand the dots here..." ]

        , FA.Record { maybeExtension, attrs }:
            translatePatternRecord env pos maybeExtension attrs

        , FA.Binop op:
            error env pos [ "compiler bug, Binop should not be here" ]

        , FA.BinopChain precedence sepList:

            if precedence == Op.precedence_tuple then
                sepList
                >> translateTuple env.ro (translateRawPattern env __) __
                >> onOk fn recordAttrs:

                CA.PatternRecord pos CA.Complete recordAttrs
                >> Ok
            else if precedence == Op.precedence_cons then
                sepList
                >> FA.sepToList
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


        , FA.LiteralText l:
            Ok << CA.PatternLiteralText pos l

        , FA.LiteralNumber isPercent l:
            translateNumber env.ro isPercent CA.PatternLiteralNumber pos l

        # Stuff that's not valid for patterns

        , FA.Statements stats:
            error env pos [ "WAT" ]

        , FA.Fn args body:
            error env pos [ "Can't pattern match on functions. =(" ]

        , FA.UnopCall unop expr:
            error env pos [ "This op can't be used in pattern matching" ]

        , FA.If _:
            error env pos [ "if..then can't be used in pattern matching" ]

        , FA.Try _:
            error env pos [ "try..as can't be used in pattern matching" ]


#
# Statement
#
translateStatements as fn Env, [FA.Statement]: Res CA.Expression =
    fn env, stats:

    try stats as
        , []:
            CoreTypes.noneValue
            >> CA.Constructor Pos.G __
            >> Ok

        , [ FA.Evaluation faExpression ]:
            translateExpression env faExpression

        , FA.Evaluation faExpr :: tail:
            faExpr
            >> translateExpression env __
            >> onOk fn caExpr:

            caDef as CA.ValueDef =
                {
                , uni = Imm
                , pattern = CA.PatternAny Pos.G Nothing Nothing
                , native = False
                , body = caExpr

                # TODO Do we need these here?
                , directTypeDeps = Dict.empty
                , directConsDeps = Dict.empty
                , directValueDeps = Dict.empty
                }

            tail
            >> translateStatements env __
            >> onOk fn acc:

            CA.LetIn caDef acc
            >> Ok

        , FA.ValueDef fa :: tail:
            fa
            >> translateDefinition False env __
            >> onOk fn newEnv & caDef:

            tail
            >> translateStatements newEnv __
            >> onOk fn acc:

            CA.LetIn caDef acc
            >> Ok

        , FA.AliasDef fa :: tail:
            At pos _ = fa.name
            error env pos [ "Aliases can be declared only in the root scope" ]

        , FA.UnionDef fa :: tail:
            At pos _ = fa.name
            error env pos [ "Types can be declared only in the root scope" ]


##
#- Expression
#
translateExpression as fn Env, FA.Expression: Res CA.Expression =
    fn env, (FA.Expression pos expr_):

    try expr_ as
        , FA.LiteralNumber isPercent str:
            translateNumber env.ro isPercent CA.LiteralNumber pos str

        , FA.LiteralText v:
            Ok << CA.LiteralText pos v

        , FA.Statements stats:
            translateStatements env stats

        , FA.Variable { maybeType, word }:
            translateVariable env pos maybeType word

        , FA.Fn faParams faBody:
            faParams
            >> List.mapRes (translateParameter env __) __
            >> onOk fn caParams:

            env
            >> List.forRes __ caParams fn par, envX:
                try par as
                    , CA.ParameterPattern uni pa: pa
                    , CA.ParameterRecycle p name: CA.PatternAny p (Just name) Nothing
                >> insertPatternNames False __ envX

            >> onOk fn localEnv:

            faBody
            >> translateExpression localEnv __
            >> onOk fn caBody:

            CA.Fn pos caParams caBody
            >> Ok

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

        , FA.Call faRef faArgs:
            faRef
            >> translateExpression env __
            >> onOk fn caRef:

            faArgs
            >> translateArgumentsAndPlaceholders pos env __
            >> onOk fn (caArgs & wrap):

            CA.Call pos caRef caArgs
            >> wrap
            >> Ok

        , FA.If { condition, true, false }:
            translateExpression env condition >> onOk fn c:
            translateExpression env true >> onOk fn t:
            translateExpression env false >> onOk fn f:
            {
            , condition = c
            , true = t
            , false = f
            }
            >> CA.If pos __
            >> Ok

        , FA.UnopCall opId faOperand:
            try opId as
                , Op.UnopUnique: error env pos [ "can't use ! here because REASONS" ]
                , Op.UnopRecycle: error env pos [ "can recycle only in function calls!" ]
                , Op.UnopPlus: translateExpression env faOperand
                , Op.UnopMinus:
                    faOperand
                    >> translateExpression env __
                    >> onOk fn caOperand:

                    CA.Call pos (CA.Variable pos (RefGlobal Prelude.unaryMinus.usr)) [CA.ArgumentExpression caOperand]
                    >> Ok

        , FA.Binop op:
            CA.Variable pos (RefGlobal op.usr)
            >> Ok

        , FA.BinopChain group sepList:
            translateBinopChain env pos group sepList

        , FA.Record { maybeExtension, attrs }:
            translateRecord env pos maybeExtension attrs

        , FA.List faDotsAndItems:

            rev =
                List.reverse faDotsAndItems

            try rev as
                , []:
                    CA.Constructor pos CoreTypes.nil
                    >> Ok

                , (hasDots & head) :: rest:
                    if List.any Tuple.first rest then
                        error env pos [ "can use dots only on the last element (for now?)" ]

                    else
                        init & revItems =
                            if hasDots then
                                head & rest
                            else
                                FA.Expression pos (FA.List []) & rev

                        translateExpression env init
                        >> onOk fn caInit:

                        caInit >> List.forRes __ revItems fn (_ & faItem), acc:
                            translateExpression env faItem
                            >> onOk fn caItem:

                            CA.Call pos (CA.Constructor pos CoreTypes.cons) [CA.ArgumentExpression caItem, CA.ArgumentExpression acc]
                            >> Ok

        , FA.Try { value, patterns }:

            translatePatternAndStatements as fn (FA.Expression & FA.Expression): Res (Uniqueness & CA.Pattern & CA.Expression) =
                fn ( faPattern & faExpression ):

                faPattern
                >> translateFullPattern env __
                >> onOk fn (uni & caPattern):

                env
                >> insertPatternNames False caPattern __
                >> onOk fn localEnv:

                faExpression
                >> translateExpression localEnv __
                >> onOk fn block:

                uni & caPattern & block
                >> Ok

            translateExpression env value
            >> onOk fn caValue:

            patterns
            >> List.mapRes translatePatternAndStatements __
            >> onOk fn patternsAndExpressions:

            CA.Try pos { value = caValue, patternsAndExpressions }
            >> Ok

        , _:
            error env pos [ "something's wrong here...", toHuman expr_ ]


insertPatternNames as fn Bool, CA.Pattern, Env: Res Env =
    fn isRoot, pattern, env:

    List.forRes env.values (CA.patternNames pattern) fn paName, vs:
        try Dict.get paName.name vs as
            , Just duplicateName:
                error env paName.pos [
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
                    Dict.insert paName.name { pos = paName.pos, isRoot } vs
                    >> Ok
                else
                    error env paName.pos [
                        , "There is already a global variable named `" .. paName.name .. "`."
                        , "You need to find a different name, or modify modules.sp"
                        ]

    >> onOk fn values:
    Ok { env with values }


translateArgumentsAndPlaceholders as fn Pos, Env, [FA.Expression]: Res ([CA.Argument] & (fn CA.Expression: CA.Expression)) =
    fn pos, env, faArgs:

    insertArg =
        fn faArg, ({ caPars, caArgs, arity }):

        FA.Expression p faArg_ =
            faArg

        try faArg_ as

            , FA.ArgumentPlaceholder:
                name =
                    Text.fromNumber arity

                {
                , caPars = CA.ParameterPlaceholder name arity :: caPars
                , caArgs = CA.ArgumentExpression (CA.Variable p (RefLocal name)) :: caArgs
                , arity = arity + 1
                }
                >> Ok

            , _:
                translateArgument env faArg
                >> onOk fn caArg:

                {
                , caPars = caPars
                , caArgs = caArg :: caArgs
                , arity = arity + 1
                }
                >> Ok

    {
    , caPars = []
    , caArgs = []
    , arity = 0
    }
    >> List.forRes __ faArgs insertArg
    >> onOk fn ({ caPars, caArgs, arity }):

    wrap =
        if caPars == [] then
            identity
        else
            fn call: CA.Fn pos (List.reverse caPars) call

    List.reverse caArgs & wrap
    >> Ok


translateVariable as fn Env, Pos, Maybe FA.Expression, Token.Word: Res CA.Expression =
    fn env, pos, maybeType, word:

    try word.modifier as
        , Token.NameStartsWithDot:
            if word.isUpper or word.maybeModule /= Nothing then
                error env pos [ "record attribute names must start with a lowercase letter" ]
            else
                try env.maybeShorthandTarget as
                    , Nothing:
                        error env pos [
                            , "Record update shorthands must be used inside a record update such as"
                            , "    { aRecord with anAttribute = doSomethingWith ." .. Text.join "." word.attrPath .. " }"
                            , "but we are not inside a record update!"
                            ]

                    , Just shorthandTarget:
                        shorthandTarget
                        >> List.for __ (word.name :: word.attrPath) (fn attrName, expr: CA.RecordAccess pos attrName expr)
                        >> Ok

        , Token.NameNoModifier:
            if word.isUpper then
                if word.attrPath /= [] then
                    error env pos [ "something's wrong with the lexer?" ]
                else
                    resolveToConstructorUsr env.ro pos word.maybeModule word.name
                    >> onOk fn usr:

                    CA.Constructor pos usr
                    >> Ok

            else
                isRoot =
                    try Dict.get word.name env.values as
                        , Nothing: True
                        , Just paName: paName.isRoot

                resolveToValueRef env.ro pos isRoot word.maybeModule word.name
                >> onOk fn usr:

                CA.Variable pos usr
                >> List.for __ word.attrPath (CA.RecordAccess pos __ __)
                >> Ok


translateParameter as fn Env, FA.Expression: Res CA.Parameter =
    fn env, fa:

    FA.Expression pos faExpr = fa

    maybeRecycle =
        try faExpr as
            , FA.UnopCall Op.UnopRecycle (FA.Expression p faOperand):
                try faOperand as
                    , FA.Variable { maybeType = Nothing, word }:
                        Ok (Just word)
                    , _:
                        error env p [ "@ should be followed by a variable name to recycle!" ]

            , _:
                Ok Nothing

    maybeRecycle
    >> onOk fn maybeWord:

    try maybeWord as
        , Just word:
            isValid = word.modifier == Token.NameNoModifier and not word.isUpper and word.maybeModule == Nothing and word.attrPath == []

            if not isValid then
                error env pos [ "I was expecting a local variable name here... =|" ]
            else
                CA.ParameterRecycle pos word.name
                >> Ok

        , Nothing:
            translateFullPattern env fa
            >> onOk fn (uni & ca):

            CA.ParameterPattern uni ca
            >> Ok


translateNumber as fn ReadOnly, Bool, (fn Pos, Number: a), Pos, Text: Res a =
    fn ro, isPercent, constructor, pos, numberAsText:

    try Text.toNumber (Text.replace "_" "" numberAsText) as
        , Nothing:
            erroro ro pos
                [
                , "invalid number: `" .. numberAsText .. "`"
                , "TODO link to documentation on valid number formats"
                ]

        , Just n:
            Ok << constructor pos (if isPercent then n / 100 else n)


translateRecord as fn Env, Pos, Maybe (Maybe FA.Expression), [FA.RecordAttribute]: Res CA.Expression =
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

            CA.Record pos Nothing caAttrs
            >> Ok

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
                # TODO ----> This desugaring needs to be done by TypeCheck, because MakeCanonical can't infer its uniqueness
                , uni = Imm
                , pattern = CA.PatternAny Pos.G (Just varName) Nothing
                , native = False
                , body = caExt

                # TODO populate deps?
                , directTypeDeps = Dict.empty
                , directConsDeps = Dict.empty
                , directValueDeps = Dict.empty
                }

            caAttrs
            >> CA.Record pos (Just var) __
            >> CA.LetIn def __
            >> Ok


translateAndInsertRecordAttribute as fn Env, FA.RecordAttribute, Dict Text CA.Expression: Res (Dict Text CA.Expression) =
    fn env, attr, caAttrsAccum:

    translateAttributeName env.ro attr.name
    >> onOk fn (pos & caName & maybeFaType):

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
    fn env, faExpr:

    try faExpr as
        , FA.Expression _ (FA.UnopCall Op.UnopRecycle (FA.Expression pos faOperand)):
            try faOperand as
                , FA.Variable { maybeType, word }:
                    if maybeType /= Nothing then
                        error env pos [ "Sorry, at least for now annotations are not supported here" ]
                    else if word.maybeModule /= Nothing then
                        error env pos [ "Only values declared inside a function scope can be mutated!" ]
                    else if word.modifier /= Token.NameNoModifier then
                        error env pos [ "This can't start with ." ]
                    else if word.isUpper then
                        error env pos [ "Can't recycle constructors" ]
                    else
                        CA.ArgumentRecycle pos word.name word.attrPath
                        >> Ok

                , _:
                    error env pos [ "I can recycle only variables!" ]

        , _:
            faExpr
            >> translateExpression env __
            >> onOk fn caExpr:

            CA.ArgumentExpression caExpr
            >> Ok


translateBinopChain as fn Env, Pos, Int, FA.SepList Op.Binop FA.Expression: Res CA.Expression =
    fn env, pos, group, opChain:
    if group == Op.precedence_pipe then
        resolvePipe env pos opChain
        >> onOk (translateExpression env __)

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


resolvePipe as fn Env, Pos, FA.SepList Op.Binop FA.Expression: Res FA.Expression =
    fn env, pos, opChain:

    if FA.sepList_allSeps (fn sep: sep.usr == Prelude.sendRight.usr) opChain then
        #
        #   head >> b >> c >> d    --->   d (c (b head))
        #
        head & sepTail =
            opChain

        List.for head sepTail fn sep & faExp, acc:
            (FA.Expression p _) = faExp
            FA.Expression p (FA.Call faExp [acc])
        >> Ok

    else if FA.sepList_allSeps (fn sep: sep.usr == Prelude.sendLeft.usr) opChain then
        #
        #   head << b << c << d    --->   head (b (c d))
        #
        last & body =
            FA.sepList_reverse opChain

        List.for last body fn sep & faExp, acc:
            (FA.Expression p _) = faExp
            FA.Expression p (FA.Call faExp [acc])
        >> Ok

    else
        error env pos [ "Mixing `>>` and `<<` is ambiguous. Use parens!" ]


translateTupleExpression as fn Env, Pos, FA.SepList Op.Binop FA.Expression: Res CA.Expression =
    fn env, pos, one & sepTail:

    try sepTail as
        , []:
            translateExpression env one

        , [_ & two]:
            translateExpression env one >> onOk fn first:
            translateExpression env two >> onOk fn second:
            Dict.empty
                >> Dict.insert "first" first __
                >> Dict.insert "second" second __
                >> CA.Record pos Nothing __
                >> Ok

        , [_ & two, _ & three]:
             translateExpression env one >> onOk fn first:
             translateExpression env two >> onOk fn second:
             translateExpression env three >> onOk fn third:
             Dict.empty
                 >> Dict.insert "first" first __
                 >> Dict.insert "second" second __
                 >> Dict.insert "third" third __
                 >> CA.Record pos Nothing __
                 >> Ok

        , _:
            error env pos [ "Tuples can't have more than 3 items, use a record instead." ]


translateComparison as fn Env, Pos, FA.SepList Op.Binop FA.Expression: Res CA.Expression =
    fn env, pos, opChain:

    try opChain.second as
        , []: translateExpression env opChain.first

        , [ sep & second ]: translateRightAssociativeBinopChain env pos opChain

        , [ firstSep & second, ...moar]:
            if FA.sepList_allSeps (sameDirectionAs firstSep __) opChain then
                # TODO expand `a < b < c` to `a < b and b < c` without calculating b twice
                error env pos [ "TODO: not (yet) implemented: compops expansion" ]

            else
                # TODO actually list the seps
                error env pos [ "can't mix comparison ops with different direction" ]


translateLogical as fn Env, Pos, FA.SepList Op.Binop FA.Expression: Res CA.Expression =
    fn env, pos, opChain:

    allSame =
        FA.sepList_allSeps (fn sep: sep.usr == Prelude.and_.usr) opChain
        or
        FA.sepList_allSeps (fn sep: sep.usr == Prelude.or_.usr) opChain

    if allSame then
        translateRightAssociativeBinopChain env pos opChain
    else
        error env pos [ "Mixing `and` and `or` is ambiguous. Use parens!" ]


translateMutop as fn Env, Pos, FA.SepList Op.Binop FA.Expression: Res CA.Expression =
    fn env, pos, left & sepTail:
    try sepTail as

        , []:
            translateExpression env left

        , [op & right]:
            translateExpression env (makeBinop pos left op right)

        , _:
            error env pos [ "mutops can't be chained" ]


sameDirectionAs as fn Op.Binop, Op.Binop: Bool =
    fn a, b:
    if a.symbol == b.symbol then
        True

    else
        try a.symbol as
            , ">":
                b.symbol == ">="

            , ">=":
                b.symbol == ">"

            , "<":
                b.symbol == "<="

            , "<=":
                b.symbol == "<"

            , _:
                False


translateRightAssociativeBinopChain as fn Env, Pos, FA.SepList Op.Binop FA.Expression: Res CA.Expression =
    fn env, pos, opChain:

    intToVariable as fn Pos, Int: FA.Expression =
        fn p, n:
        {
        , maybeType = Nothing
        , word = {
            , modifier = Token.NameNoModifier
            , isUpper = False
            , maybeModule = Nothing
            , name = Text.fromNumber n
            , attrPath = []
            }
        }
        >> FA.Variable
        >> FA.Expression p __

    rec as fn Int, FA.SepList Op.Binop FA.Expression: Int & FA.Expression =
        fn placeholdersCount, rawLeft & opsAndRight:

        count & left =
            try rawLeft as
                , FA.Expression p FA.ArgumentPlaceholder:
                    placeholdersCount + 1 & intToVariable p placeholdersCount

                , _:
                    placeholdersCount & rawLeft

        try opsAndRight as
            , []:
                count & left

            , [op & right, ...tail]:
                newCount & newRight =
                    rec count (right & tail)

                # TODO this pos is wrong, we need the op's pos
                newCount & makeBinop pos left op newRight

    cnt & expr =
        rec 0 opChain

    if cnt == 0 then
        expr
    else
        cnt - 1
        >> List.range 0 __
        >> List.map (intToVariable pos __) __
        >> FA.Fn __ expr
        >> FA.Expression pos __

    >> translateExpression env __


makeBinop as fn Pos, FA.Expression, Op.Binop, FA.Expression: FA.Expression =
    fn pos, left, op, right:

    FA.Expression pos (FA.Call (FA.Expression pos (FA.Binop op)) [left, right])



#
# Type
#


translateAndInsertRecordAttributeType as fn ReadOnly, FA.RecordAttribute, Dict Name CA.RawType: Res (Dict Name CA.RawType) =
    fn ro, faAttr, caAttrs:

    translateAttributeName ro faAttr.name
    >> onOk fn (pos & name & maybeFaType):

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


translateNamedType as fn ReadOnly, Pos, Token.Word, [CA.RawType]: Res CA.RawType =
    fn ro, pos, word, caArgs:

    if word.modifier /= Token.NameNoModifier then
        erroro ro pos [ "I was expecting a type name here =|" ]
    else if word.attrPath /= [] then
        erroro ro pos [ "Type names have no attributes to access" ]
    else
        resolveToTypeUsr ro pos word.maybeModule word.name
        >> onOk fn usr:

        CA.TypeNamed pos usr caArgs
        >> Ok


translateTypeVariable as fn ReadOnly, Pos, Token.Word: Res Name =
    fn ro, pos, word:

    if word.modifier /= Token.NameNoModifier then
        erroro ro pos [ "I was expecting a type variable name here =|" ]
    else if word.attrPath /= [] then
        erroro ro pos [ "Type variables have no attributes to access" ]
    else if word.maybeModule /= Nothing then
        erroro ro pos [ "No point it getting tyvars from modules?" ]
    else
        Ok word.name


translateTypeFunctionParameter as fn ReadOnly, FA.Expression: Res CA.ParType =
    fn ro, expression:

    FA.Expression _ expr_ =
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

    FA.Expression pos expr_ = expr

    try expr_ as

        , FA.UnopCall Op.UnopUnique e:
            Uni & e
            >> Ok

        , FA.Poly numberAsString e:
            try Text.toNumber numberAsString as
                , Nothing: erroro ro pos [ "I need an integer number here" ]
                , Just n: Depends n & e >> Ok

        , _:
            Imm & expr
            >> Ok



translateFullType as fn ReadOnly, FA.Expression: Res CA.FullType =
    fn ro, expr:

    expr
    >> translatePoly ro __
    >> onOk fn (uni & e):

    translateRawType ro e
    >> onOk fn raw:

    { uni, raw }
    >> Ok


translateRawType as fn ReadOnly, FA.Expression: Res CA.RawType =
    fn ro, (FA.Expression pos expr_):

    try expr_ as
        , FA.Variable { maybeType, word }:
            if maybeType /= Nothing then
                erroro ro pos [ "Can't really specify the type of a type." ]
            else if word.isUpper then
                translateNamedType ro pos word []
            else
                translateTypeVariable ro pos word
                >> onOk fn tyvarName:

                CA.TypeAnnotationVariable pos tyvarName
                >> Ok

        , FA.Call (FA.Expression refPos ref) faArgs:
            try ref as
                , FA.Variable { maybeType = Nothing, word }:
                    faArgs
                    >> List.mapRes (translateRawType ro __) __
                    >> onOk fn caArgs:

                    translateNamedType ro refPos word caArgs

                , _:
                    erroro ro refPos [ "I was expecting a named type here" ]

        , FA.List dotsAndItems:
            try dotsAndItems as
                , []:
                    erroro ro pos [ "You need to specify the type of the List items" ]

                , [ hasDots & faItem ]:
                    if hasDots then
                        erroro ro pos [ "No need to use dots here" ]
                    else
                        translateRawType ro faItem
                        >> onOk fn caItem:

                        CoreTypes.list caItem
                        >> Ok

                , _:
                    erroro ro pos [ "List items must all have the same type, so you can specify only one type" ]

        , FA.Record { maybeExtension, attrs }:
            if maybeExtension /= Nothing then
                erroro ro pos [ "Experimentally, extensible type annotations are disabled" ]

            else
                Dict.empty
                >> List.forRes __ attrs (translateAndInsertRecordAttributeType ro __ __)
                >> onOk fn caAttrs:

                caAttrs
                >> CA.TypeRecord pos __
                >> Ok

        , FA.Fn faParams faReturn:
            faParams
            >> List.mapRes (translateTypeFunctionParameter ro __) __
            >> onOk fn caParams:

            faReturn
            >> translateFullType ro __
            >> onOk fn caReturn:

            CA.TypeFn pos caParams caReturn
            >> Ok

        , FA.BinopChain precedence sepList:

            if precedence == Op.precedence_tuple then
                sepList
                >> translateTuple ro (translateRawType ro __) __
                >> onOk fn recordAttrs:

                CA.TypeRecord pos recordAttrs
                >> Ok
            else
                erroro ro pos [ "This operator can't be used in type definitions", toHuman expr_ ]

        , _:
            # TODO: do all other constructors explicitly
            erroro ro pos [ "Not sure what's up with this type =|", toHuman expr_ ]


#
# Union constructor
#


translateConstructor as fn CA.RawType, USR, FA.Expression, Dict Name CA.Constructor & Env: Res (Dict Name CA.Constructor & Env) =
    fn unionType, unionUsr, (FA.Expression pos expr_), constructors & env:

    zzz =
        try expr_ as
            , FA.Variable var:
                var & [] >> Ok

            , FA.Call (FA.Expression _ (FA.Variable var)) pars:
                var & pars >> Ok

            , _:
                error env pos [ "I was expecting a constructor name here" ]

    zzz
    >> onOk fn ({ maybeType, word } & faPars):

    isValidName =
      word.modifier == Token.NameNoModifier
      and
      word.isUpper
      and
      word.maybeModule == Nothing
      and
      word.attrPath == []

    if not isValidName then
        error env pos [ "I need just an Uppercase word here" ]

    else if Dict.member word.name constructors then
        # TODO "union $whatever has two constructors with the same name!"
        error env pos [ "constructor " .. word.name .. " is duplicate" ]

    else
        faPars
        >> List.mapRes (translateRawType env.ro __) __
        >> onOk fn ins:

        env
        >> insertPatternNames True (CA.PatternAny pos (Just word.name) Nothing) __
        >> onOk fn newEnv:

        c as CA.Constructor = {
            , pos
            , typeUsr = unionUsr
            , ins
            , out = unionType
            }

        Dict.insert word.name c constructors & newEnv
        >> Ok


translateTypeParameter as fn ReadOnly, At Token.Word: Res (Name & Pos) =
    fn ro, (At pos word):

    if word.modifier /= Token.NameNoModifier then
        erroro ro pos [ "Can't start with ." ]
    else if word.isUpper then
        erroro ro pos [ "type params must start with a lowercase letter" ]
    else if word.maybeModule /= Nothing then
        erroro ro pos [ "why modules here?" ]
    else if word.attrPath /= [] then
        erroro ro pos [ "why attrs here?" ]
    else
        Ok (word.name & pos)


translateTypeName as fn ReadOnly, At Token.Word: Res Name =
    fn ro, (At pos word):

    if word.modifier /= Token.NameNoModifier then
        erroro ro pos [ "Can't start with ." ]
    else if not word.isUpper then
        erroro ro pos [ "type names must start with Uppercase letter" ]
    else if word.maybeModule /= Nothing then
        erroro ro pos [ "why modules here?" ]
    else if word.attrPath /= [] then
        erroro ro pos [ "why attrs here?" ]
    else
        Ok word.name


#
# Module
#


insertRootStatement as fn FA.Statement, CA.Module & Env: Res (CA.Module & Env) =
    fn faStatement, caModule & env:

    try faStatement as
        , FA.Evaluation (FA.Expression pos _):
            error env pos [ "Root Evaluations don't really do much =|" ]

        , FA.ValueDef d:
            d
            >> translateDefinition True env __
            >> onOk fn newEnv & def:

            if def.uni /= Imm then
                error env (CA.patternPos def.pattern) [ "Unique values can be declared only inside functions." ]

            else
                # Patterns contain position, so they are unique and don't need to be checked for duplication
                { caModule with valueDefs = Dict.insert def.pattern def .valueDefs } & newEnv
                >> Ok

        , FA.AliasDef fa:
            translateTypeName env.ro fa.name
            >> onOk fn name:

            if Dict.member name caModule.aliasDefs or Dict.member name caModule.unionDefs then
                At pos _ = fa.name
                error env pos [ name .. " declared twice!" ]

            else
                fa.args
                >> List.mapRes (translateTypeParameter env.ro __) __
                >> onOk fn caPars:

                # TODO check that args are not duplicate

                fa.type
                >> translateRawType env.ro __
                >> onOk fn type:

                aliasDef as CA.AliasDef =
                    {
                    , usr = USR env.ro.umr name
                    , pars = caPars
                    , type
                    , directTypeDeps = typeDeps type Set.empty
                    }

                { caModule with aliasDefs = Dict.insert name aliasDef .aliasDefs } & env
                >> Ok

        , FA.UnionDef fa:
            At pos _ = fa.name

            translateTypeName env.ro fa.name
            >> onOk fn name:

            if Dict.member name caModule.aliasDefs or Dict.member name caModule.unionDefs then
                error env pos [ name .. " declared twice!" ]

            else
                fa.args
                >> List.mapRes (translateTypeParameter env.ro __) __
                >> onOk fn caPars:

                # TODO check that args are not duplicate

                usr =
                    USR env.ro.umr name

                type =
                    caPars
                    >> List.map (fn n & p: CA.TypeAnnotationVariable p n) __
                    >> CA.TypeNamed pos usr __

                Dict.empty & env
                >> List.forRes __ fa.constructors (translateConstructor type usr __ __)
                >> onOk fn constructors & newEnv:

                unionDef as CA.UnionDef = {
                    , usr
                    , pars = caPars
                    , constructors
                    # I could probably break the deps by constructor, but would it be much useful in practice?
                    , directTypeDeps = Dict.for Set.empty constructors (fn k, c, z: List.for z c.ins typeDeps)
                    }

                { caModule with unionDefs = Dict.insert name unionDef .unionDefs } & newEnv
                >> Ok


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
    , stripLocations
    }
    >> Compiler/Parser.textToFormattableModule
    >> Result.onOk fn faModule:
    translateModule ro faModule

