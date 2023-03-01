
alias Params =
    {
    , meta as Meta
    , stripLocations as Bool
    , source as Meta.Source
    , name as Name
    }


#textToCanonicalModule as fn Params, Text: Res CA.Module =
#    fn pars, code:
#    todo "textToCanonicalModule"

#

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
    # This keeps track of values that are declared within the
    # scope of a function, so they don't need to be expanded with the module name.
    #
    , nonRootValues as Dict Text { pos as Pos, maybeAnnotation as Maybe CA.RawType }
    #
    # TODO This is used to tell the user that definitions must be in order
    #
    #, futureNonRootValues as Dict Text Pos
    #
    #
    , ro as ReadOnly

    , nonFn as Set Text
    }


alias ReadOnly =
    {
    , currentModule as UMR
    , meta as Meta
    }


initEnv as fn ReadOnly: Env =
    fn ro:
    {
    , maybeShorthandTarget = Nothing
    , nextGeneratedVariableName = 0
    , nonRootValues = Dict.empty
    #, futureNonRootValues = Dict.empty
    , ro = ro
    , nonFn = Dict.empty
    }


#
# Errors
#


error as fn Pos, [Text]: Res a =
    fn pos, msg:
    Error.res pos fn errorEnv: msg


#
# Names resolution
#

maybeForeignUsr as fn (fn Meta: Dict Text USR), ReadOnly, Maybe Name, Name: Maybe USR =
    fn getter, ro, maybeModule, name:

    try maybeModule as
        , Just moduleName:
            try Dict.get moduleName ro.meta.moduleVisibleAsToUmr as
                , Just umr:
                    Just << USR umr name

                , Nothing:
                    # TODO should this produce an error instead?
                    # i.e., does ro.meta.moduleVisibleAsToUmr contain *all* modules, aliased or not?
                    #RefGlobal (USR Meta.SourcePlaceholder moduleName name)
#                    List.each (Dict.keys ro.meta.moduleVisibleAsToUmr) x:
#                        log "*" x
                    todo << "!!resolveToUsr can't find the module: " .. moduleName .. " (for: " .. name .. ")"

        , Nothing:
            Dict.get name (getter ro.meta)


resolveToUsr as fn (fn Meta: Dict Text USR), ReadOnly, Maybe Name, Name: USR =
    fn getter, ro, maybeModule, name:

    maybeForeignUsr getter ro maybeModule name
        >> Maybe.withDefault (USR ro.currentModule name) __


resolveToValueRef as fn ReadOnly, Bool, Maybe Name, Name: Ref =
    fn ro, declaredInsideFunction, maybeModule, name:

    try maybeForeignUsr (fn m: m.globalValues) ro maybeModule name as
        , Just usr:
            RefGlobal usr

        , Nothing:
            if declaredInsideFunction then
                RefLocal name

            else
                RefGlobal << USR ro.currentModule name


resolveToTypeUsr as fn ReadOnly, Maybe Name, Name: USR =
    resolveToUsr (fn m: m.globalTypes) __ __ __


resolveToConstructorUsr as fn ReadOnly, Maybe Name, Name: USR =
    resolveToUsr (fn m: m.globalValues) __ __ __


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

        , CA.PatternAny _ { maybeName = _, maybeAnnotation = Just type }:
            { deps with types = typeDeps type .types }

        , CA.PatternAny _ { maybeName = _, maybeAnnotation = Nothing }:
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


addUnivarId as fn Uniqueness, Dict UnivarId None: Dict UnivarId None =
    fn uni, acc:

    try uni as
        , Depends id: Dict.insert id None acc
        , _: acc


addPar as fn CA.ParType, Dict UnivarId None: Dict UnivarId None =
    fn parType, acc:

    try parType as
        , CA.ParRe raw: addRawTypeUnivars raw acc
        , CA.ParSp full:
            acc
            >> addUnivarId full.uni __
            >> addRawTypeUnivars full.raw __


addRawTypeUnivars as fn CA.RawType, Dict UnivarId None: Dict UnivarId None =
    fn raw, acc:

    try raw as
        , CA.TypeNamed _ _ args: List.for acc args addRawTypeUnivars
        , CA.TypeRecord _ attrs: Dict.for acc attrs (fn k, v, a: addRawTypeUnivars v a)
        , CA.TypeAnnotationVariable _ _: acc
        , CA.TypeError _: acc
        , CA.TypeFn _ pars out:
            acc
            >> addUnivarId out.uni __
            >> addRawTypeUnivars out.raw __
            >> List.for __ pars addPar


addPatternUnivars as fn CA.Pattern, Dict UnivarId None: Dict UnivarId None =
    fn pattern, acc:

    try pattern as
        , CA.PatternConstructor _ _ args: List.for acc args addPatternUnivars
        , CA.PatternRecord _ _ attrs: Dict.for acc attrs (fn k, v, a: addPatternUnivars v a)
        , CA.PatternAny _ { maybeName, maybeAnnotation = Just rawType }: addRawTypeUnivars rawType acc
        , _: acc


translateDefinition as fn Bool, Env, FA.ValueDef: Res CA.ValueDef =
    fn isRoot, env, fa:

    fa.pattern
    >> translateFullPattern env __
    >> onOk fn (uni & pattern):

    fa.nonFn
    >> List.mapRes translateTypeParameter __
    >> onOk fn nonFn:

    univars as Dict UnivarId None =
        Dict.empty
        >> addUnivarId uni __
        >> addPatternUnivars pattern __

    tyvars as Dict Name CA.Tyvar =
        pattern
        >> CA.patternTyvars
        >> Dict.map (fn tyvarName, pos: { allowFunctions = List.all (fn (At _ name): name /= tyvarName) nonFn }) __

    # TODO: todo: check that the typeclasses are consistent with those declared in parentEnv

    # TODO: check that nonFn contains only names actually used in the annotation?

    nonRootValues1 =
        if isRoot then
            env.nonRootValues

        else
            Dict.join (CA.patternNames pattern) env.nonRootValues

    localEnv0 =
        { env with
        , nonRootValues = nonRootValues1
        }

    fa.body
    >> translateExpression localEnv0 __
    >> onOk fn body:

    deps =
        if isRoot then
            deps_init >> patternDeps pattern __ >> expressionDeps body __
        else
            deps_init

    {
    , uni
    , pattern
    , tyvars
    , univars
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
translateAttributeName as fn FA.Expression: Res (Pos & Name & Maybe FA.Expression) =
    fn (FA.Expression pos expr_):

    try expr_ as
        , FA.Variable { maybeType, word }:

            if word.modifier /= Token.NameNoModifier then
                error pos [ "attribute names can't start with a dot" ]
            else if word.isUpper then
                error pos [ "attribute names must be lower case" ]
            else if word.maybeModule /= Nothing then
                error pos [ "attribute names must be single words" ]
            else if word.attrPath /= [] then
                error pos [ "attribute names can't contain dots" ]
            else
                pos & word.name & maybeType >> Ok

        , _:
            error pos [ "Expecting an attribute name here" ]


translatePatternConstructor as fn Env, Pos, Token.Word, [CA.Pattern]: Res CA.Pattern =
    fn env, pos, word, args:

    if word.modifier /= Token.NameNoModifier then
        error pos [ "Constructor names cannot have modifiers" ]
    else if word.attrPath /= [] then
        error pos [ "Constructors don't have attributes" ]
    else
        args
        >> CA.PatternConstructor pos (resolveToConstructorUsr env.ro word.maybeModule word.name) __
        >> Ok


translatePatternAny as fn Env, Pos, Maybe FA.Expression, Token.Word: Res CA.Pattern =
    fn env, pos, maybeFaType, word:

    if word.modifier /= Token.NameNoModifier then
        error pos [ "Record access shorthands" ]
    else if word.attrPath /= [] then
        error pos [ "To access attributes in pattern matching use { with theAttributeName = theVariableName }" ]
    else if word.maybeModule /= Nothing then
        error pos [ "You can't access modules here..." ]
    else
        resultMaybeRaw as Res (Maybe CA.RawType) =
            try maybeFaType as
                , Just faType:
                    faType
                    >> translateRawType env.ro __
                    >> Result.map Just __

                , Nothing:
                    Ok Nothing

        resultMaybeRaw
        >> onOk fn maybeRaw:

        maybeName =
            if word.name == "_" then Nothing else Just word.name

        Ok << CA.PatternAny pos { maybeName, maybeAnnotation = maybeRaw }


insertAttribute as fn Env, FA.RecordAttribute, Dict Name CA.Pattern: Res (Dict Name CA.Pattern) =
    fn env, attr, caAttrs:

    # { x }
    # { x = pattern }
    # { x as Type }

    translateAttributeName attr.name
    >> onOk fn (pos & caName & maybeFaType):

    if Dict.member caName caAttrs then
        error pos [ "duplicate attribute name in pattern: " .. caName ]

    else
        try attr.maybeExpr & maybeFaType as
            , Just _ & Just (FA.Expression typePos _):
                error typePos [ "if you want to annotate the attribute, use { x = y as TheType }" ]

            , Nothing & Just faType:
                translateRawType env.ro faType
                >> onOk fn caType:

                caAttrs
                >> Dict.insert caName (CA.PatternAny pos { maybeName = Just caName, maybeAnnotation = Just caType }) __
                >> Ok

            , Just faPattern & Nothing:
                faPattern
                >> translateRawPattern env __
                >> onOk fn caPattern:

                caAttrs
                >> Dict.insert caName caPattern __
                >> Ok

             , Nothing & Nothing:
                caAttrs
                >> Dict.insert caName (CA.PatternAny pos { maybeName = Just caName, maybeAnnotation = Nothing }) __
                >> Ok


translatePatternRecord as fn Env, Pos, Maybe (Maybe FA.Expression), [{ name as FA.Expression, maybeExpr as Maybe FA.Expression }]: Res CA.Pattern =
    fn env, pos, maybeMaybeExt, attrs:

    zzz =
        try maybeMaybeExt as
            , Just (Just (FA.Expression p expr_)):
                error p [ "Can't extend patterns" ]

            , Just Nothing:
                # { with attr1 = ... }
                Ok CA.Partial

            , Nothing:
                # { attr1 = ... }
                Ok CA.Complete

    zzz
    >> onOk fn completeness:

    Dict.empty
    >> List.forRes __ attrs (insertAttribute env __ __)
    >> Result.map (fn x: CA.PatternRecord pos completeness x) __


translateTuple as fn (fn FA.Expression: Res ca), FA.SepList Op.Binop FA.Expression: Res (Dict Name ca) =
    fn translate, sepList:

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
            error pos [ "tuples can be only of size 2 or 3, use a record instead" ]


translateFullPattern as fn Env, FA.Expression: Res (Uniqueness & CA.Pattern) =
    fn env, expr:

    expr
    >> translatePoly __
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
                    error pos [ "Pattern constructors can't have type annotations" ]
                else
                    translatePatternConstructor env pos word []
            else
                translatePatternAny env pos maybeType word

        , FA.Call (FA.Expression pos ref) faArgs:
            try ref as
                , FA.Variable { maybeType, word }:
                    if not word.isUpper then
                        error pos [ "I need an uppercase constructor name here" ]
                    else if maybeType /= Nothing then
                        error pos [ "Constructors can't be annotated (yet? would it make sense?)" ]
                    else
                        faArgs
                        >> List.mapRes (translateRawPattern env __) __
                        >> onOk fn caPars:
                        translatePatternConstructor env pos word caPars

                , _:
                    error pos [ "I was expecting a constructor name here" ]

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

                , [lastHasDots & FA.Expression pos lastFaExpr, ...reversedFaRest]:
                    if List.any Tuple.first reversedFaRest then
                        error pos [ "only the last item in a list can have ... triple dots" ]

                    else if not lastHasDots then
                        reversedFaItems
                        >> List.mapRes (fn (hasDots & expr): translateRawPattern env expr) __
                        >> onOk fn reversedCaItems:

                        List.for (CA.PatternConstructor pos CoreTypes.nil []) reversedCaItems pushItem
                        >> Ok

                    else
                        reversedFaRest
                        >> List.mapRes (fn (hasDots & expr): translateRawPattern env expr) __
                        >> onOk fn reversedCaRest:

                        try lastFaExpr as
                            , FA.Variable { maybeType, word }:
                                translatePatternAny env pos maybeType word
                                >> onOk fn caInit:

                                List.for caInit reversedCaRest pushItem
                                >> Ok

                            , _:
                                error pos [ "sorry, I don't understand the dots here..." ]

        , FA.Record { maybeExtension, attrs }:
            translatePatternRecord env pos maybeExtension attrs

        , FA.Binop Op.Tuple sepList:
            sepList
            >> translateTuple (translateRawPattern env __) __
            >> onOk fn recordAttrs:

            CA.PatternRecord pos CA.Complete recordAttrs
            >> Ok

        , FA.Binop Op.Cons sepList:
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
                    error pos [ "should not happen: empty cons pattern" ]

        , FA.Binop opPrecedence sepList:
            error pos [ "This binop can't be used in pattern matching" ]

        , FA.LiteralText l:
            Ok << CA.PatternLiteralText pos l

        , FA.LiteralNumber l:
            translateNumber CA.PatternLiteralNumber pos l

        # Stuff that's not valid for patterns

        , FA.Statements stats:
            error pos [ "WAT" ]

        , FA.Fn args body:
            error pos [ "Can't pattern match on functions. =(" ]

        , FA.Unop unop expr:
            error pos [ "This op can't be used in pattern matching" ]

        , FA.If _:
            error pos [ "if..then can't be used in pattern matching" ]

        , FA.Try _:
            error pos [ "try..as can't be used in pattern matching" ]


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
                , pattern = CA.PatternAny Pos.G { maybeName = Nothing, maybeAnnotation = Nothing }
                , native = False
                , body = caExpr
                , tyvars = Dict.empty
                , univars = Dict.empty

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
            >> onOk fn caDef:

            newEnv as Env =
                { env with nonRootValues = Dict.join (CA.patternNames caDef.pattern) .nonRootValues }

            tail
            >> translateStatements newEnv __
            >> onOk fn acc:

            CA.LetIn caDef acc
            >> Ok

        , FA.AliasDef fa :: tail:
            At pos _ = fa.name
            error pos [ "Aliases can be declared only in the root scope" ]

        , FA.UnionDef fa :: tail:
            At pos _ = fa.name
            error pos [ "Types can be declared only in the root scope" ]


##
#- Expression
#
translateExpression as fn Env, FA.Expression: Res CA.Expression =
    fn env, (FA.Expression pos expr_):

    try expr_ as
        , FA.LiteralNumber str:
            translateNumber CA.LiteralNumber pos str

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

            zzz as Res Env =
                env >> List.forRes __ caParams fn par, envX:
                    names =
                        try par as
                            , CA.ParameterPattern uni pa: CA.patternNames pa
                            , CA.ParameterRecycle _ name: Dict.ofOne name { pos, maybeAnnotation = Nothing }

                    duplicates =
                        Dict.intersect names envX.nonRootValues >> Dict.keys

                    if duplicates /= [] then
                        error pos [ "parameters shadows these values: " .. Text.join "," duplicates ]
                    else
                        Ok { envX with nonRootValues = Dict.join names .nonRootValues }

            zzz
            >> onOk fn localEnv:

            faBody
            >> translateExpression localEnv __
            >> onOk fn caBody:

            CA.Fn pos caParams caBody
            >> Ok

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

        , FA.Unop opId faOperand:
            try opId as
                , Op.UnopUnique: error pos [ "can't use ! here because REASONS" ]
                , Op.UnopRecycle: error pos [ "can recycle only in function calls!" ]
                , Op.UnopPlus: translateExpression env faOperand
                , Op.UnopMinus:
                    faOperand
                    >> translateExpression env __
                    >> onOk fn caOperand:

                    CA.Call pos (CA.Variable pos (RefGlobal Prelude.unaryMinus.usr)) [CA.ArgumentExpression caOperand]
                    >> Ok

        , FA.Binop group sepList:
            translateBinops env pos group sepList

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
                        error pos [ "can use dots only on the last element (for now?)" ]

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

                faExpression
                >> translateExpression { env with nonRootValues = Dict.join (CA.patternNames caPattern) env.nonRootValues } __
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
            error pos [ "something's wrong here...", toHuman expr_ ]


translateArgumentsAndPlaceholders as fn Pos, Env, [FA.Expression]: Res ([CA.Argument] & (fn CA.Expression: CA.Expression)) =
    fn pos, env, faArgs:

    insertArg =
        fn faArg, ({ caPars, caArgs, arity }):

        FA.Expression pos faArg_ =
            faArg

        try faArg_ as

            , FA.ArgumentPlaceholder:
                name =
                    Text.fromNumber arity

                {
                , caPars = CA.ParameterPlaceholder name arity :: caPars
                , caArgs = CA.ArgumentExpression (CA.Variable pos (RefLocal name)) :: caArgs
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
                error pos [ "record attribute names must start with a lowercase letter" ]
            else
                try env.maybeShorthandTarget as
                    , Nothing:
                        error pos [
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
                    error pos [ "something's wrong with the lexer?" ]
                else
                    resolveToConstructorUsr env.ro word.maybeModule word.name
                        >> CA.Constructor pos __
                        >> Ok

            else
                declaredInsideFunction =
                    Dict.member word.name env.nonRootValues

                resolveToValueRef env.ro declaredInsideFunction word.maybeModule word.name
                >> CA.Variable pos __
                >> List.for __ word.attrPath (CA.RecordAccess pos __ __)
                >> Ok


translateParameter as fn Env, FA.Expression: Res CA.Parameter =
    fn env, fa:

    FA.Expression pos faExpr = fa

    maybeRecycle =
        try faExpr as
            , FA.Unop Op.UnopRecycle (FA.Expression p faOperand):
                try faOperand as
                    , FA.Variable { maybeType = Nothing, word }:
                        Ok (Just word)
                    , _:
                        error p [ "@ should be followed by a variable name to recycle!" ]

            , _:
                Ok Nothing

    maybeRecycle
    >> onOk fn maybeWord:

    try maybeWord as
        , Just word:
            isValid = word.modifier == Token.NameNoModifier and not word.isUpper and word.maybeModule == Nothing and word.attrPath == []

            if not isValid then
                error pos [ "I was expecting a local variable name here... =|" ]
            else
                CA.ParameterRecycle pos word.name
                >> Ok

        , Nothing:
            translateFullPattern env fa
            >> onOk fn (uni & ca):

            CA.ParameterPattern uni ca
            >> Ok


translateNumber as fn (fn Pos, Number: a), Pos, Text: Res a =
    fn constructor, pos, numberAsText:

    try Text.toNumber numberAsText as
        , Nothing:
            error pos [
                , "invalid number: `" .. numberAsText .. "`"
                , "TODO link to documentation on valid number formats"
                ]

        , Just n:
            Ok << constructor pos n


translateRecord as fn Env, Pos, Maybe (Maybe FA.Expression), [FA.RecordAttribute]: Res CA.Expression =
    fn env, pos, maybeMaybeExtension, attrs:

    zzz as Res (Maybe CA.Expression) =
        try maybeMaybeExtension as
            , Just (Just ext): translateExpression env ext >> Result.map Just __
            , Just Nothing: error pos [ "I need to know what record you are updating" ]
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
                , pattern = CA.PatternAny Pos.G { maybeName = Just varName, maybeAnnotation = Nothing }
                , native = False
                , body = caExt
                , tyvars = Dict.empty
                , univars = Dict.empty

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

    translateAttributeName attr.name
    >> onOk fn (pos & caName & maybeFaType):

    if Dict.member caName caAttrsAccum then
        error pos [ "duplicate attribute: " .. caName ]
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
        , FA.Expression _ (FA.Unop Op.UnopRecycle (FA.Expression pos faOperand)):
            try faOperand as
                , FA.Variable { maybeType, word }:
                    if maybeType /= Nothing then
                        error pos [ "Sorry, at least for now annotations are not supported here" ]
                    else if word.maybeModule /= Nothing then
                        error pos [ "Only values declared inside a function scope can be mutated!" ]
                    else if word.modifier /= Token.NameNoModifier then
                        error pos [ "This can't start with ." ]
                    else if word.isUpper then
                        error pos [ "Can't recycle constructors" ]
                    else
                        CA.ArgumentRecycle pos word.name word.attrPath
                        >> Ok

                , _:
                    error pos [ "I can recycle only variables!" ]

        , _:
            faExpr
            >> translateExpression env __
            >> onOk fn caExpr:

            CA.ArgumentExpression caExpr
            >> Ok


translateBinops as fn Env, Pos, Op.Precedence, FA.SepList Op.Binop FA.Expression: Res CA.Expression =
    fn env, pos, group, ( firstItem & firstTail ):
    try firstTail as
        , []:
            translateExpression env firstItem

        , (firstSep & secondItem) :: []:
            try group as
                , Op.Tuple:
                    translateExpression env firstItem >> onOk fn first:
                    translateExpression env secondItem >> onOk fn second:
                    Dict.empty
                        >> Dict.insert "first" first __
                        >> Dict.insert "second" second __
                        >> CA.Record pos Nothing __
                        >> Ok

                , _:
                    translateSimpleBinop env pos firstItem firstSep secondItem

        , (firstSep & secondItem) :: (secondSep & thirdItem) :: thirdTail:

            secondTail as [Op.Binop & FA.Expression] =
                (secondSep & thirdItem) :: thirdTail

            try group as
                , Op.Comparison:
                    if notAllSeparators (sameDirectionAs firstSep __) secondTail then
                        # TODO actually list the seps
                        error pos [ "can't mix comparison ops with different direction" ]

                    else
                        # TODO expand `a < b < c` to `a < b and b < c` without calculating b twice
                        error pos [ "NI compops expansion" ]

                , Op.Logical:
                    if notAllSeparators (fn x: x == firstSep) secondTail then
                        error pos [ "Mixing `and` and `or` is ambiguous. Use parens!" ]

                    else
                        translateBinopSepList_rightAssociative env pos firstItem firstTail

                , Op.Tuple:
                    if thirdTail /= [] then
                        error pos [ "Tuples can't have more than 3 items, use a record instead." ]

                    else
                         translateExpression env firstItem >> onOk fn first:
                         translateExpression env secondItem >> onOk fn second:
                         translateExpression env thirdItem >> onOk fn third:
                         Dict.empty
                             >> Dict.insert "first" first __
                             >> Dict.insert "second" second __
                             >> Dict.insert "third" third __
                             >> CA.Record pos Nothing __
                             >> Ok

                , Op.Pipe:
                    if notAllSeparators (fn x: x == firstSep) secondTail then
                        error pos [ "Mixing pipes is ambigous. Use parens." ]

                    else if firstSep.associativity == Op.Right then
                        translateBinopSepList_rightAssociative env pos firstItem firstTail

                    else
                        translateBinopSepList_leftAssociative env pos firstItem firstTail

                , Op.Mutop:
                    error pos [ "mutops can't be chained" ]

                , _:
                    translateBinopSepList_rightAssociative env pos firstItem firstTail


notAllSeparators as fn (fn sep: Bool), [ sep & item]: Bool =
    fn f, ls:
    try ls as
        , []:
            False

        , (sep & item) :: tail:
            if f sep then
                notAllSeparators f tail

            else
                True


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


translateBinopSepList_rightAssociative as fn Env, Pos, FA.Expression, [ Op.Binop & FA.Expression ]: Res CA.Expression =
    fn env, pos, left, opsAndRight:
    translateExpression env left >> onOk fn caLeft:
    try opsAndRight as
        , []:
            Ok caLeft

        , (op & right) :: tail:
            translateBinopSepList_rightAssociative env pos right tail
            >> onOk fn caRight:

            makeBinop pos (CA.ArgumentExpression caLeft) op (CA.ArgumentExpression caRight)


translateBinopSepList_leftAssociative as fn Env, Pos, FA.Expression, [ Op.Binop & FA.Expression ]: Res CA.Expression =
    fn env, pos, leftAccum, opsAndRight:

    translateExpression env leftAccum >> onOk fn caLeftAccum:
    translateBinopSepListRec env pos caLeftAccum opsAndRight


translateBinopSepListRec as fn Env, Pos, CA.Expression, [ Op.Binop & FA.Expression ]: Res CA.Expression =
    fn env, pos, leftAccum, opsAndRight:
    try opsAndRight as
        , []:
            Ok leftAccum

        , (op & faRight) :: tail:
            translateArgument env faRight >> onOk fn caRight:
            makeBinop pos (CA.ArgumentExpression leftAccum) op caRight
            >> onOk fn binop:
            translateBinopSepListRec env pos binop tail


[# Unlike other ML languages, the left operand is the _second_ argument

`a + b` == `((+) b) a`

#]
makeBinop as fn Pos, CA.Argument, Op.Binop, CA.Argument: Res CA.Expression =
    fn pos, left, op, right:

    if op.symbol == Prelude.sendRight.symbol then
        # arg3 >> fun arg1 arg2
        try right as
            , CA.ArgumentExpression ref:
                CA.Call pos ref [left]
                >> Ok

            , CA.ArgumentRecycle _ _ _:
                error pos [ "Can't >> to a recyclable" ]

    else if op.symbol == Prelude.sendLeft.symbol then
        # arg3 >> fun arg1 arg2
        try left as
            , CA.ArgumentExpression ref:
                CA.Call pos ref [right]
                >> Ok

            , CA.ArgumentRecycle _ _ _:
                error pos [ "Can't << to a recyclable" ]

    else
        CA.Call pos (CA.Variable pos (RefGlobal op.usr)) [left, right]
        >> Ok


translateSimpleBinop as fn Env, Pos, FA.Expression, Op.Binop, FA.Expression: Res CA.Expression =
    fn env, pos, left, op, right:
    translateArgument env left >> onOk fn l:
    translateArgument env right >> onOk fn r:
    makeBinop pos l op r


#
# Type
#


translateAndInsertRecordAttributeType as fn ReadOnly, FA.RecordAttribute, Dict Name CA.RawType: Res (Dict Name CA.RawType) =
    fn ro, faAttr, caAttrs:

    translateAttributeName faAttr.name
    >> onOk fn (pos & name & maybeFaType):

    if Dict.member name caAttrs then
        error pos [ "Duplicate attribute name: " .. name ]
    else
        try maybeFaType as
            , Nothing:
                error pos [ "I need to see the type of this attribute, `" .. name .. " as TheType`" ]

            , Just faType:
                faType
                >> translateRawType ro __
                >> onOk fn caType:

                if faAttr.maybeExpr /= Nothing then
                    error pos [ "I'm expecting a type here; `=` is for assignign values" ]
                else
                    caAttrs
                    >> Dict.insert name caType __
                    >> Ok


translateNamedType as fn ReadOnly, Pos, Token.Word, [CA.RawType]: Res CA.RawType =
    fn ro, pos, word, caArgs:

    if word.modifier /= Token.NameNoModifier then
        error pos [ "I was expecting a type name here =|" ]
    else if word.attrPath /= [] then
        error pos [ "Type names have no attributes to access" ]
    else
        CA.TypeNamed pos (resolveToTypeUsr ro word.maybeModule word.name) caArgs
        >> Ok


translateTypeVariable as fn Pos, Token.Word: Res Name =
    fn pos, word:

    if word.modifier /= Token.NameNoModifier then
        error pos [ "I was expecting a type variable name here =|" ]
    else if word.attrPath /= [] then
        error pos [ "Type variables have no attributes to access" ]
    else if word.maybeModule /= Nothing then
        error pos [ "No point it getting tyvars from modules?" ]
    else
        Ok word.name


translateTypeFunctionParameter as fn ReadOnly, FA.Expression: Res CA.ParType =
    fn ro, expression:

    FA.Expression _ expr_ =
        expression

    try expr_ as
        , FA.Unop Op.UnopRecycle faOperand:
            faOperand
            >> translateRawType ro __
            >> Result.map CA.ParRe __

        , _:
            expression
            >> translateFullType ro __
            >> Result.map CA.ParSp __


translatePoly as fn FA.Expression: Res (Uniqueness & FA.Expression) =
    fn expr:

    FA.Expression pos expr_ = expr

    try expr_ as

        , FA.Unop Op.UnopUnique e:
            Uni & e
            >> Ok

        , FA.Poly numberAsString e:
            try Text.toNumber numberAsString as
                , Nothing: error pos [ "I need an integer number here" ]
                , Just n: Depends n & e >> Ok

        , _:
            Imm & expr
            >> Ok



translateFullType as fn ReadOnly, FA.Expression: Res CA.FullType =
    fn ro, expr:

    expr
    >> translatePoly
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
                error pos [ "Can't really specify the type of a type." ]
            else if word.isUpper then
                translateNamedType ro pos word []
            else
                translateTypeVariable pos word
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
                    error refPos [ "I was expecting a named type here" ]

        , FA.List dotsAndItems:
            try dotsAndItems as
                , []:
                    error pos [ "You need to specify the type of the List items" ]

                , [ hasDots & faItem ]:
                    if hasDots then
                        error pos [ "No need to use dots here" ]
                    else
                        translateRawType ro faItem
                        >> onOk fn caItem:

                        CoreTypes.list caItem
                        >> Ok

                , _:
                    error pos [ "List items must all have the same type, so you can specify only one type" ]

        , FA.Record { maybeExtension, attrs }:
            if maybeExtension /= Nothing then
                error pos [ "Experimentally, extensible type annotations are disabled" ]

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

        , FA.Binop Op.Tuple sepList:
            sepList
            >> translateTuple (translateRawType ro __) __
            >> onOk fn recordAttrs:

            CA.TypeRecord pos recordAttrs
            >> Ok

        , _:
            # TODO: do all other constructors explicitly
            error pos [ "Not sure what's up with this type =|", toHuman expr_ ]


#
# Union constructor
#


translateConstructor as fn ReadOnly, CA.RawType, USR, FA.Expression, Dict Name CA.Constructor: Res (Dict Name CA.Constructor) =
    fn ro, unionType, unionUsr, (FA.Expression pos expr_), constructors:

    zzz =
        try expr_ as
            , FA.Variable var:
                var & [] >> Ok

            , FA.Call (FA.Expression _ (FA.Variable var)) pars:
                var & pars >> Ok

            , _:
                error pos [ "I was expecting a constructor name here" ]

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
        error pos [ "I need just an Uppercase word here" ]

    else if Dict.member word.name constructors then
        # TODO "union $whatever has two constructors with the same name!"
        error pos [ "constructor " .. word.name .. " is duplicate" ]

    else
        faPars
        >> List.mapRes (translateRawType ro __) __
        >> onOk fn ins:

        c as CA.Constructor =
            {
            , pos
            , typeUsr = unionUsr
            , ins
            , out = unionType
            }

        constructors
        >> Dict.insert word.name c __
        >> Ok


translateTypeParameter as fn At Token.Word: Res (At Name) =
    fn (At pos word):

    if word.modifier /= Token.NameNoModifier then
        error pos [ "Can't start with ." ]
    else if word.isUpper then
        error pos [ "type params must start with a lowercase letter" ]
    else if word.maybeModule /= Nothing then
        error pos [ "why modules here?" ]
    else if word.attrPath /= [] then
        error pos [ "why attrs here?" ]
    else
        Ok (At pos word.name)


translateTypeName as fn At Token.Word: Res Name =
    fn (At pos word):

    if word.modifier /= Token.NameNoModifier then
        error pos [ "Can't start with ." ]
    else if not word.isUpper then
        error pos [ "type names must start with Uppercase letter" ]
    else if word.maybeModule /= Nothing then
        error pos [ "why modules here?" ]
    else if word.attrPath /= [] then
        error pos [ "why attrs here?" ]
    else
        Ok word.name


#
# Module
#


insertRootStatement as fn ReadOnly, FA.Statement, CA.Module: Res (CA.Module) =
    fn ro, faStatement, caModule:

    try faStatement as
        , FA.Evaluation (FA.Expression pos _):
            error pos [ "Root Evaluations don't really do much =|" ]

        , FA.ValueDef d:
            d
            >> translateDefinition True (initEnv ro) __
            >> onOk fn def:

            if def.uni /= Imm then
                error (CA.patternPos def.pattern) [ "Unique values can be declared only inside functions." ]


            # TODO check duplicates!!!!

            else
                # Patterns contain position, so they are unique and don't need to be checked for duplication
                # Names duplication will be checked when rootValuesAndConstructors is populated
                Ok { caModule with valueDefs = Dict.insert def.pattern def .valueDefs }

        , FA.AliasDef fa:
            translateTypeName fa.name
            >> onOk fn name:

            if Dict.member name caModule.aliasDefs or Dict.member name caModule.unionDefs then
                At pos _ = fa.name
                error pos [ name .. " declared twice!" ]

            else
                fa.args
                >> List.mapRes translateTypeParameter __
                >> onOk fn caPars:

                # TODO check that args are not duplicate

                fa.type
                >> translateRawType ro __
                >> onOk fn type:

                aliasDef as CA.AliasDef =
                    {
                    , usr = USR ro.currentModule name
                    , pars = caPars
                    , type
                    , directTypeDeps = typeDeps type Set.empty
                    }

                Ok { caModule with aliasDefs = Dict.insert name aliasDef .aliasDefs }

        , FA.UnionDef fa:
            At pos _ = fa.name

            translateTypeName fa.name
            >> onOk fn name:

            if Dict.member name caModule.aliasDefs or Dict.member name caModule.unionDefs then
                error pos [ name .. " declared twice!" ]

            else
                fa.args
                >> List.mapRes translateTypeParameter __
                >> onOk fn caPars:

                # TODO check that args are not duplicate

                usr =
                    USR ro.currentModule name

                type =
                    caPars
                    >> List.map (fn (At p n): CA.TypeAnnotationVariable p n) __
                    >> CA.TypeNamed pos usr __

                Dict.empty
                >> List.forRes __ fa.constructors (translateConstructor ro type usr __ __)
                >> onOk fn constructors:

                unionDef as CA.UnionDef =
                    {
                    , usr
                    , pars = caPars
                    , constructors
                    # I could probably break the deps by constructor, but would it be much useful in practice?
                    , directTypeDeps = Dict.for Set.empty constructors (fn k, c, z: List.for z c.ins typeDeps)
                    }

                Ok { caModule with unionDefs = Dict.insert name unionDef .unionDefs }


translateModule as fn ReadOnly, Text, UMR, FA.Module: Res (CA.Module) =
    fn ro, asText, umr, faModule:

    Debug.benchStart None

    module =
        CA.initModule asText umr

    # Add all definitions
    module
    >> List.forRes __ faModule (insertRootStatement ro __ __)
    >> btw Debug.benchStop "translateModule" __


textToCanonicalModule as fn Params, Text: Res CA.Module =
    fn pars, code:

    ro as Compiler/MakeCanonical.ReadOnly =
        {
        , currentModule = UMR pars.source pars.name
        , meta = pars.meta
        }

    umr as UMR =
        UMR pars.source pars.name

    code
    >> Compiler/Parser.textToFormattableModule
        {
        , stripLocations = pars.stripLocations
        , moduleName = pars.name
        }
        __
    >> Result.onOk fn faModule:
    translateModule ro code umr faModule

#]
