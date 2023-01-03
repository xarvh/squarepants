

#maybe_mapRes as (a: Result e b): Maybe a: Result e (Maybe b) =
#    f: maybeA:
#
#    try maybeA as
#        Nothing:
#            Ok Nothing
#
#        Just a:
#            Result.map Just (f a)




alias Params = {
    , meta as Meta
    , stripLocations as Bool
    , source as Meta.Source
    , name as Name
    }


[#
    `Env` is immutable and depends only on the parent scopes.
    Which means, each scope will have a different one.

    `State` is mutable and is reset per each module.
    We don't have state for now.

#]
alias Env = {
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
    , nonRootValues as Dict Text { pos as Pos, isUnique as Bool, maybeAnnotation as Maybe CA.Type }
    #
    # TODO This is used to tell the user that definitions must be in order
    #
    #, futureNonRootValues as Dict Text Pos
    #
    #
    , ro as ReadOnly

    , nonFn as Set Text
    }


alias ReadOnly = {
    , currentModule as UMR
    , meta as Meta
    }


initEnv as ReadOnly: Env =
    ro: {
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


error as Pos: [Text]: Res a =
    pos: msg:
    Error.res pos errorEnv: msg



#
#
#
typeSetUni as Pos: UniqueOrImmutable: CA.Type: Res CA.Type =
    pos: uni: type:

    CA.Type p_ type_ =
        type

    ok = t: CA.Type p_ t >> Ok

    try type_ as
        CA.TypeNamed usr _ pars:
            CA.TypeNamed usr (CA.UniIsFixed uni) pars
            >> ok

        CA.TypeFn _ _:
            if uni == Imm then
                Ok type
            else
                error pos [ "Can't set TypeFn to Uni", toHuman type ]

        CA.TypeRecord _ attrs:
            CA.TypeRecord uni attrs
            >> ok

        CA.TypeAnnotationVariable _ name:
            CA.TypeAnnotationVariable uni name
            >> ok

        CA.TypeError:
            Ok type


#
# Names resolution
#

maybeForeignUsr as (Meta: Dict Text USR): ReadOnly: Maybe Name: Name: Maybe USR =
    getter: ro: maybeModule: name:

    try maybeModule as
        Just moduleName:
            try Dict.get moduleName ro.meta.moduleVisibleAsToUmr as
                Just umr:
                    Just << USR umr name

                Nothing:
                    # TODO should this produce an error instead?
                    # i.e., does ro.meta.moduleVisibleAsToUmr contain *all* modules, aliased or not?
                    #RefGlobal (USR Meta.SourcePlaceholder moduleName name)
#                    List.each (Dict.keys ro.meta.moduleVisibleAsToUmr) x:
#                        log "*" x
                    todo << "!!resolveToUsr can't find the module: " .. moduleName .. " (for: " .. name .. ")"

        Nothing:
            Dict.get name (getter ro.meta)


resolveToUsr as (Meta: Dict Text USR): ReadOnly: Maybe Name: Name: USR =
    getter: ro: maybeModule: name:

    maybeForeignUsr getter ro maybeModule name
        >> Maybe.withDefault (USR ro.currentModule name)


resolveToValueRef as ReadOnly: Bool: Maybe Name: Name: Ref =
    ro: declaredInsideFunction: maybeModule: name:

    try maybeForeignUsr (m: m.globalValues) ro maybeModule name as
        Just usr:
            RefGlobal usr

        Nothing:
            if declaredInsideFunction then
                RefLocal name

            else
                RefGlobal << USR ro.currentModule name


resolveToTypeUsr as ReadOnly: Maybe Name: Name: USR =
    resolveToUsr (m: m.globalTypes)


resolveToConstructorUsr as ReadOnly: Maybe Name: Name: USR =
    resolveToUsr (m: m.globalValues)


#
# Dependencies
#


typeDeps as CA.Type: Set USR: Set USR =
    (CA.Type _ type_): acc:
    try type_ as
        CA.TypeNamed usr _ args: acc >> Set.insert usr >> List.for args typeDeps
        CA.TypeAnnotationVariable _ _: acc
        CA.TypeFn params to: acc >> typeDeps to >> List.for params ((_ & f): typeDeps f)
        CA.TypeRecord _ attrs: Dict.for attrs (k: typeDeps) acc
        CA.TypeError: acc


alias Deps = {
    , types as Set USR
    , cons as Set USR
    , values as Set USR
    }


deps_init = {
    , types = Set.empty
    , cons = Set.empty
    , values = Set.empty
    }


patternDeps as CA.Pattern: Deps: Deps =
    pattern: deps:

    try pattern as

        CA.PatternConstructor _ usr ps:
            List.for ps patternDeps { deps with cons = Set.insert usr .cons }

        CA.PatternRecord _ completeness ps:
            Dict.for ps (k: patternDeps) deps

        CA.PatternAny _ { isUnique = _, maybeName = _, maybeAnnotation = Just type }:
            { deps with types = typeDeps type .types }

        CA.PatternAny _ { isUnique = _, maybeName = _, maybeAnnotation = Nothing }:
            deps

        CA.PatternLiteralNumber _ _:
           deps

        CA.PatternLiteralText _ _:
           deps


expressionDeps as CA.Expression: Deps: Deps =
    expression: deps:

    try expression as
        CA.LiteralNumber _ _:
            deps

        CA.LiteralText _ _:
            deps

        CA.Variable _ (RefGlobal usr):
            { deps with values = Set.insert usr .values }

        CA.Variable _ _:
            deps

        CA.Constructor _ usr:
            { deps with cons = Set.insert usr .cons }

        CA.Fn _ pars body:
            deps
            >> List.for pars parameterDeps
            >> expressionDeps body

        CA.Record _ Nothing exprByName:
            deps
            >> Dict.for exprByName (name: expressionDeps)

        CA.Record _ (Just expr) exprByName:
            deps
            >> expressionDeps expr
            >> Dict.for exprByName (name: expressionDeps)

        CA.Record _ _ exprByName:
            deps
            >> Dict.for exprByName (name: expressionDeps)

        CA.RecordAccess _ _ e:
            expressionDeps e deps

        CA.Call _ e0 args:
            deps
            >> expressionDeps e0
            >> List.for args argumentDeps

        CA.If _ args:
            deps
            >> expressionDeps args.condition
            >> expressionDeps args.true
            >> expressionDeps args.false

        CA.Try _ { value, patternsAndExpressions }:
            deps
            >> expressionDeps value
            >> List.for patternsAndExpressions ((p & b): d: d >> patternDeps p >> expressionDeps b)

        CA.LetIn valueDef e:
            deps
            >> patternDeps valueDef.pattern
            >> expressionDeps valueDef.body
            >> expressionDeps e


argumentDeps as CA.Argument: Deps: Deps =
    arg: deps:

    try arg as
        CA.ArgumentExpression e: expressionDeps e deps
        CA.ArgumentRecycle _ _ _: deps


parameterDeps as CA.Parameter: Deps: Deps =
    par: deps:

    try par as
        CA.ParameterPattern pa: patternDeps pa deps
        CA.ParameterRecycle _ _: deps


#
# Definition
#


translateDefinition as Bool: Env: FA.ValueDef: Res (CA.ValueDef) =
    isRoot: env: fa:

    fa.pattern
    >> translatePattern env
    >> onOk pattern:

    fa.nonFn
    >> List.mapRes translateTypeParameter
    >> onOk nonFn:

    tyvars as Dict Name CA.TypeClasses =
        pattern
        >> CA.patternTyvars
        >> Dict.map tyvarName: pos:
            {
            , allowFunctions = List.all ((At pos name): name /= tyvarName) nonFn
            , allowUniques = False
            }

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
    >> translateExpression localEnv0
    >> onOk body:

    deps =
        if isRoot then
            deps_init >> patternDeps pattern >> expressionDeps body
        else
            deps_init

    {
    , pattern
    , tyvars
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
translateAttributeName as FA.Expression: Res (Pos & Name & Maybe FA.Expression) =
    (FA.Expression pos expr_):

    try expr_ as
        FA.Variable { maybeType, word }:

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

        _:
            error pos [ "Expecting an attribute name here" ]


translatePatternConstructor as Env: Pos: Token.Word: [CA.Pattern]: Res CA.Pattern =
    env: pos: word: args:

    if word.modifier /= Token.NameNoModifier then
        error pos [ "Constructor names cannot have modifiers" ]
    else if word.attrPath /= [] then
        error pos [ "Constructors don't have attributes" ]
    else
        args
        >> CA.PatternConstructor pos (resolveToConstructorUsr env.ro word.maybeModule word.name)
        >> Ok


translatePatternAny as Env: Pos: Bool: Maybe FA.Expression: Token.Word: Res CA.Pattern =
    env: pos: isUnique: maybeFaType: word:

    if word.modifier /= Token.NameNoModifier then
        error pos [ "Record access shorthands" ]
    else if word.attrPath /= [] then
        error pos [ "To access attributes in pattern matching use { with theAttributeName = theVariableName }" ]
    else if word.maybeModule /= Nothing then
        error pos [ "You can't access modules here..." ]
    else
        getMaybeCaType as Res (Maybe CA.Type) =
            try maybeFaType as
                Just faType:
                    faType
                    >> translateType env.ro
                    >> Result.map Just

                Nothing:
                    Ok Nothing

        getMaybeCaType
        >> onOk maybeCaType:

        maybeName =
            if word.name == "_" then Nothing else Just word.name

        Ok << CA.PatternAny pos { isUnique, maybeName, maybeAnnotation = maybeCaType }


insertAttribute as Env: FA.RecordAttribute: Dict Name CA.Pattern: Res (Dict Name CA.Pattern) =
    env: attr: caAttrs:

    # { x }
    # { x = pattern }
    # { x as Type }

    translateAttributeName attr.name
    >> onOk (pos & caName & maybeFaType):

    if Dict.member caName caAttrs then
        error pos [ "duplicate attribute name in pattern: " .. caName ]

    else
        try attr.maybeExpr & maybeFaType as
            Just _ & Just (FA.Expression typePos _):
                error typePos [ "if you want to annotate the attribute, use { x = y as TheType }" ]

            Nothing & Just faType:
                translateType env.ro faType
                >> onOk caType:

                caAttrs
                >> Dict.insert caName (CA.PatternAny pos { isUnique = False, maybeName = Just caName, maybeAnnotation = Just caType })
                >> Ok

            Just faPattern & Nothing:
                faPattern
                >> translatePattern env
                >> onOk caPattern:

                caAttrs
                >> Dict.insert caName caPattern
                >> Ok

             Nothing & Nothing:
                caAttrs
                >> Dict.insert caName (CA.PatternAny pos { isUnique = False, maybeName = Just caName, maybeAnnotation = Nothing })
                >> Ok


translatePatternRecord as Env: Pos: Maybe (Maybe FA.Expression): [{ name as FA.Expression, maybeExpr as Maybe FA.Expression }]: Res CA.Pattern =
    env: pos: maybeMaybeExt: attrs:

    zzz =
        try maybeMaybeExt as
            Just (Just (FA.Expression p expr_)):
                error p [ "Can't extend patterns" ]

            Just Nothing:
                # { with attr1 = ... }
                Ok CA.Partial

            Nothing:
                # { attr1 = ... }
                Ok CA.Complete

    zzz
    >> onOk completeness:

    Dict.empty
    >> List.forRes attrs (insertAttribute env)
    >> Result.map (x: x >> CA.PatternRecord pos completeness)


translateTuple as (FA.Expression: Res ca): FA.SepList Op.Binop FA.Expression: Res (Dict Name ca) =
    translate: sepList:

    faExpressions as [FA.Expression] =
        FA.sepToList sepList

    faExpressions
    >> List.mapRes translate
    >> onOk items:

    pos as Pos =
        List.for faExpressions ((FA.Expression pos _): Pos.range pos) Pos.G

    try items as
        [ ca1, ca2 ]:
            Dict.empty
            >> Dict.insert "first" ca1
            >> Dict.insert "second" ca2
            >> Ok

        [ ca1, ca2, ca3 ]:
            Dict.empty
            >> Dict.insert "first" ca1
            >> Dict.insert "second" ca2
            >> Dict.insert "third" ca3
            >> Ok

        _:
            error pos [ "tuples can be only of size 2 or 3, use a record instead" ]



translatePattern as Env: FA.Expression: Res CA.Pattern =
    env: (FA.Expression pos expr_):

    try expr_ as

        FA.Unop Op.UnopUnique (FA.Expression pos (FA.Variable { maybeType, word })):
            if word.isUpper then
                error pos [ "Constructor functions can't be unique" ]
            else
                translatePatternAny env pos True maybeType word

        FA.Variable { maybeType, word }:

            if word.isUpper then
                if maybeType /= Nothing then
                    error pos [ "Pattern constructors can't have type annotations" ]
                else
                    translatePatternConstructor env pos word []
            else
                translatePatternAny env pos False maybeType word

        FA.Call (FA.Expression pos ref) faArgs:
            try ref as
                FA.Variable { maybeType, word }:
                    if not word.isUpper then
                        error pos [ "I need an uppercase constructor name here" ]
                    else if maybeType /= Nothing then
                        error pos [ "Constructors can't be annotated (yet? would it make sense?)" ]
                    else
                        faArgs
                        >> List.mapRes (translatePattern env)
                        >> onOk caPars:
                        translatePatternConstructor env pos word caPars

                _:
                    error pos [ "I was expecting a constructor name here" ]

        FA.List faItems:

            reversedFaItems =
                List.reverse faItems

            pushItem as CA.Pattern: CA.Pattern: CA.Pattern =
                pattern: last:
                CA.PatternConstructor (CA.patternPos pattern) CoreTypes.cons [ pattern, last ]

            try reversedFaItems as
                []:
                    CA.PatternConstructor pos CoreTypes.nil []
                    >> Ok

                (lastHasDots & FA.Expression pos lastFaExpr) :: reversedFaRest:
                    if List.any Tuple.first reversedFaRest then
                        error pos [ "only the last item in a list can have ... triple dots" ]

                    else if not lastHasDots then
                        reversedFaItems
                        >> List.mapRes ((hasDots & expr): translatePattern env expr)
                        >> onOk reversedCaItems:

                        List.for reversedCaItems pushItem (CA.PatternConstructor pos CoreTypes.nil [])
                        >> Ok

                    else
                        reversedFaRest
                        >> List.mapRes ((hasDots & expr): translatePattern env expr)
                        >> onOk reversedCaRest:

                        try lastFaExpr as
                            FA.Variable { maybeType, word }:

                                # TODO there is no reason why this couldn't be unique
                                translatePatternAny env pos False maybeType word
                                >> onOk caInit:

                                List.for reversedCaRest pushItem caInit
                                >> Ok

                            _:
                                error pos [ "sorry, I don't understand the dots here..." ]

        FA.Record { maybeExtension, attrs }:
            translatePatternRecord env pos maybeExtension attrs

        FA.Binop Op.Tuple sepList:
            sepList
            >> translateTuple (translatePattern env)
            >> onOk recordAttrs:

            CA.PatternRecord pos CA.Complete recordAttrs
            >> Ok

        FA.Binop Op.Cons sepList:
            sepList
            >> FA.sepToList
            >> List.mapRes (translatePattern env)
            >> onOk caPas:
            try List.reverse caPas as
                last :: rest:
                    last
                    >> List.for rest (item: list: CA.PatternConstructor pos CoreTypes.cons [ item, list ])
                    >> Ok

                []:
                    error pos [ "should not happen: empty cons pattern" ]

        FA.Binop opPrecedence sepList:
            error pos [ "This binop can't be used in pattern matching" ]

        FA.LiteralText l:
            Ok << CA.PatternLiteralText pos l

        FA.LiteralNumber l:
            translateNumber CA.PatternLiteralNumber pos l

        # Stuff that's not valid for patterns

        FA.Statements stats:
            error pos [ "WAT" ]

        FA.Fn args body:
            error pos [ "Can't pattern match on functions. =(" ]

        FA.Unop unop expr:
            error pos [ "This op can't be used in pattern matching" ]

        FA.If _:
            error pos [ "if..then can't be used in pattern matching" ]

        FA.Try _:
            error pos [ "try..as can't be used in pattern matching" ]


#
# Statement
#
translateStatements as Env: [FA.Statement]: Res CA.Expression =
    env: stats:

    try stats as
        []:
            CoreTypes.noneValue
            >> CA.Constructor Pos.G
            >> Ok

        [ FA.Evaluation faExpression ]:
            translateExpression env faExpression

        FA.Evaluation faExpr :: tail:
            faExpr
            >> translateExpression env
            >> onOk caExpr:

            caDef as CA.ValueDef =
                {
                , pattern = CA.PatternAny Pos.G { isUnique = False, maybeName = Nothing, maybeAnnotation = Nothing }
                , native = False
                , body = caExpr
                , tyvars = Dict.empty

                # TODO Do we need these here?
                , directTypeDeps = Dict.empty
                , directConsDeps = Dict.empty
                , directValueDeps = Dict.empty
                }

            tail
            >> translateStatements env
            >> onOk acc:

            CA.LetIn caDef acc
            >> Ok

        FA.ValueDef fa :: tail:
            fa
            >> translateDefinition False env
            >> onOk caDef:

            newEnv as Env =
                { env with nonRootValues = Dict.join (CA.patternNames caDef.pattern) .nonRootValues }

            tail
            >> translateStatements newEnv
            >> onOk acc:

            CA.LetIn caDef acc
            >> Ok

        FA.AliasDef fa :: tail:
            At pos _ = fa.name
            error pos [ "Aliases can be declared only in the root scope" ]

        FA.UnionDef fa :: tail:
            At pos _ = fa.name
            error pos [ "Types can be declared only in the root scope" ]


##
#- Expression
#
translateExpression as Env: FA.Expression: Res CA.Expression =
    env: (At pos expr_):

    try expr_ as
        FA.LiteralNumber str:
            translateNumber CA.LiteralNumber pos str

        FA.LiteralText v:
            Ok << CA.LiteralText pos v

        FA.Statements stats:
            translateStatements env stats

        FA.Variable { maybeType, word }:
            translateVariable env pos maybeType word

        FA.Fn faParams faBody:
            faParams
            >> List.mapRes (translateParameter env)
            >> onOk caParams:

            zzz as Res Env =
                env >> List.forRes caParams par: envX:
                    names =
                        try par as
                            CA.ParameterPattern pa: CA.patternNames pa
                            CA.ParameterRecycle _ name: Dict.singleton name { pos, isUnique = True, maybeAnnotation = Nothing }

                    duplicates =
                        Dict.intersect names envX.nonRootValues >> Dict.keys

                    if duplicates /= [] then
                        error pos [ "parameters shadows these values: " .. Text.join "," duplicates ]
                    else
                        Ok { envX with nonRootValues = Dict.join names .nonRootValues }

            zzz
            >> onOk localEnv:

            faBody
            >> translateExpression localEnv
            >> onOk caBody:

            CA.Fn pos caParams caBody
            >> Ok

        FA.Call faRef faArgs:
            faRef
            >> translateExpression env
            >> onOk caRef:

            faArgs
            >> translateArgumentsAndPlaceholders pos env
            >> onOk (caArgs & wrap):

            CA.Call pos caRef caArgs
            >> wrap
            >> Ok

        FA.If { condition, true, false }:
            translateExpression env condition >> onOk c:
            translateExpression env true >> onOk t:
            translateExpression env false >> onOk f:
            {
            , condition = c
            , true = t
            , false = f
            }
            >> CA.If pos
            >> Ok

        FA.Unop opId faOperand:
            try opId as
                Op.UnopUnique: error pos [ "can't use ! here because REASONS" ]
                Op.UnopRecycle: error pos [ "can recycle only in function calls!" ]
                Op.UnopPlus: translateExpression env faOperand
                Op.UnopMinus:
                    faOperand
                    >> translateExpression env
                    >> onOk caOperand:

                    CA.Call pos (CA.Variable pos (RefGlobal Prelude.unaryMinus.usr)) [CA.ArgumentExpression caOperand]
                    >> Ok

        FA.Binop group sepList:
            translateBinops env pos group sepList

        FA.Record { maybeExtension, attrs }:
            translateRecord env pos maybeExtension attrs

        FA.List faDotsAndItems:

            rev =
                List.reverse faDotsAndItems

            try rev as
                []:
                    CA.Constructor pos CoreTypes.nil
                    >> Ok

                (hasDots & head) :: rest:
                    if List.any Tuple.first rest then
                        error pos [ "can use dots only on the last element (for now?)" ]

                    else
                        init & revItems =
                            if hasDots then
                                head & rest
                            else
                                FA.Expression pos (FA.List []) & rev

                        translateExpression env init
                        >> onOk caInit:

                        caInit >> List.forRes revItems (_ & faItem): acc:
                            translateExpression env faItem
                            >> onOk caItem:

                            CA.Call pos (CA.Constructor pos CoreTypes.cons) [CA.ArgumentExpression caItem, CA.ArgumentExpression acc]
                            >> Ok

        FA.Try { value, patterns }:

            translatePatternAndStatements as (FA.Expression & FA.Expression): Res (CA.Pattern & CA.Expression) =
                ( faPattern & faExpression ):

                faPattern
                >> translatePattern env
                >> onOk caPattern:

                faExpression
                >> translateExpression { env with nonRootValues = Dict.join (CA.patternNames caPattern) env.nonRootValues }
                >> onOk block:

                caPattern & block
                >> Ok

            translateExpression env value
            >> onOk caValue:

            patterns
            >> List.mapRes translatePatternAndStatements
            >> onOk patternsAndExpressions:

            CA.Try pos { value = caValue, patternsAndExpressions }
            >> Ok




translateArgumentsAndPlaceholders as Pos: Env: [FA.Expression]: Res ([CA.Argument] & (CA.Expression: CA.Expression)) =
    pos: env: faArgs:


    insertArg =
        faArg: ({ caPars, caArgs, arity }):

        FA.Expression pos faArg_ =
            faArg

        try faArg_ as

            FA.ArgumentPlaceholder:
                name =
                    Text.fromNumber arity

                caPar as CA.Parameter =
                    {
                    , isUnique = False
                    , maybeName = Just name
                    , maybeAnnotation = Nothing
                    }
                    >> CA.PatternAny pos
                    >> CA.ParameterPattern

                caArg as CA.Argument =
                    CA.ArgumentExpression (CA.Variable pos (RefLocal name))

                {
                , caPars = caPar :: caPars
                , caArgs = caArg :: caArgs
                , arity = arity + 1
                }
                >> Ok

            _:
                translateArgument env faArg
                >> onOk caArg:

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
    >> List.forRes faArgs insertArg
    >> onOk ({ caPars, caArgs, arity }):

    wrap =
        if caPars == [] then
            identity
        else
            call: CA.Fn pos (List.reverse caPars) call

    List.reverse caArgs & wrap
    >> Ok


translateVariable as Env: Pos: Maybe FA.Expression: Token.Word: Res CA.Expression =
    env: pos: maybeType: word:

    try word.modifier as
        Token.NameStartsWithDot:
            if word.isUpper or word.maybeModule /= Nothing then
                error pos [ "record attribute names must start with a lowercase letter" ]
            else
                try env.maybeShorthandTarget as
                    Nothing:
                        error pos [
                            , "Record update shorthands must be used inside a record update such as"
                            , "    { aRecord with anAttribute = doSomethingWith ." .. Text.join "." word.attrPath .. " }"
                            , "but we are not inside a record update!"
                            ]

                    Just shorthandTarget:
                        shorthandTarget
                        >> List.for (word.name :: word.attrPath) (attrName: expr: CA.RecordAccess pos attrName expr)
                        >> Ok

        Token.NameNoModifier:
            if word.isUpper then
                if word.attrPath /= [] then
                    error pos [ "something's wrong with the lexer?" ]
                else
                    resolveToConstructorUsr env.ro word.maybeModule word.name
                        >> CA.Constructor pos
                        >> Ok

            else
                declaredInsideFunction =
                    Dict.member word.name env.nonRootValues

                resolveToValueRef env.ro declaredInsideFunction word.maybeModule word.name
                >> CA.Variable pos
                >> List.for word.attrPath (CA.RecordAccess pos)
                >> Ok


translateParameter as Env: FA.Expression: Res CA.Parameter =
    env: fa:

    FA.Expression pos faExpr = fa

    maybeRecycle =
        try faExpr as
            FA.Unop Op.UnopRecycle (FA.Expression p faOperand):
                try faOperand as
                    FA.Variable { maybeType = Nothing, word }:
                        Ok (Just word)
                    _:
                        error p [ "@ should be followed by a variable name to recycle!" ]

            _:
                Ok Nothing

    maybeRecycle
    >> onOk maybeWord:

    try maybeWord as
        Just word:
            isValid = word.modifier == Token.NameNoModifier and not word.isUpper and word.maybeModule == Nothing and word.attrPath == []

            if not isValid then
                error pos [ "I was expecting a local variable name here... =|" ]
            else
                CA.ParameterRecycle pos word.name
                >> Ok

        Nothing:
            translatePattern env fa
            >> onOk ca:

            CA.ParameterPattern ca
            >> Ok


translateNumber as (Pos: Number: a): Pos: Text: Res a =
    constructor: pos: numberAsText:

    try Text.toNumber numberAsText as
        Nothing:
            error pos [
                , "invalid number: `" .. numberAsText .. "`"
                , "TODO link to documentation on valid number formats"
                ]

        Just n:
            Ok << constructor pos n


translateRecord as Env: Pos: Maybe (Maybe FA.Expression): [FA.RecordAttribute]: Res CA.Expression =
    env: pos: maybeMaybeExtension: attrs:

    zzz as Res (Maybe CA.Expression) =
        try maybeMaybeExtension as
            Just (Just ext): translateExpression env ext >> Result.map Just
            Just Nothing: error pos [ "I need to know what record you are updating" ]
            Nothing: Ok Nothing

    zzz
    >> onOk maybeCaExt:

    try maybeCaExt as
        Nothing:
            Dict.empty
            >> List.forRes attrs (translateAndInsertRecordAttribute { env with maybeShorthandTarget = Nothing })
            >> onOk caAttrs:

            CA.Record pos Nothing caAttrs
            >> Ok

        Just caExt:
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
            >> List.forRes attrs (translateAndInsertRecordAttribute newEnv)
            >> onOk caAttrs:

            def as CA.ValueDef =
                {
                , pattern = CA.PatternAny Pos.G { isUnique = False, maybeName = Just varName, maybeAnnotation = Nothing }
                , native = False
                , body = caExt
                , tyvars = Dict.empty
                # TODO populate deps?
                , directTypeDeps = Dict.empty
                , directConsDeps = Dict.empty
                , directValueDeps = Dict.empty
                }

            caAttrs
            >> CA.Record pos (Just var)
            >> CA.LetIn def
            >> Ok


translateAndInsertRecordAttribute as Env: FA.RecordAttribute: Dict Text CA.Expression: Res (Dict Text CA.Expression) =
    env: attr: caAttrsAccum:

    translateAttributeName attr.name
    >> onOk (pos & caName & maybeFaType):

    if Dict.member caName caAttrsAccum then
        error pos [ "duplicate attribute: " .. caName ]
    else
        attr.maybeExpr
        >> Maybe.withDefault attr.name
        >> translateExpression env
        >> onOk caExpr:

        caAttrsAccum
        >> Dict.insert caName caExpr
        >> Ok


translateArgument as Env: FA.Expression: Res CA.Argument =
    env: faExpr:

    try faExpr as
        FA.Expression _ (FA.Unop Op.UnopRecycle (FA.Expression pos faOperand)):
            try faOperand as
                FA.Variable { maybeType, word }:
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

                _:
                    error pos [ "I can recycle only variables!" ]

        _:
            faExpr
            >> translateExpression env
            >> onOk caExpr:

            CA.ArgumentExpression caExpr
            >> Ok


translateBinops as Env: Pos: Op.Precedence: FA.SepList Op.Binop FA.Expression: Res CA.Expression =
    env: pos: group: ( firstItem & firstTail ):
    try firstTail as
        []:
            translateExpression env firstItem

        (firstSep & secondItem) :: []:
            try group as
                Op.Tuple:
                    translateExpression env firstItem >> onOk first:
                    translateExpression env secondItem >> onOk second:
                    Dict.empty
                        >> Dict.insert "first" first
                        >> Dict.insert "second" second
                        >> CA.Record pos Nothing
                        >> Ok

                _:
                    translateSimpleBinop env pos firstItem firstSep secondItem

        (firstSep & secondItem) :: (secondSep & thirdItem) :: thirdTail:

            secondTail as [Op.Binop & FA.Expression] =
                (secondSep & thirdItem) :: thirdTail

            try group as
                Op.Comparison:
                    if notAllSeparators (sameDirectionAs firstSep) secondTail then
                        # TODO actually list the seps
                        error pos [ "can't mix comparison ops with different direction" ]

                    else
                        # TODO expand `a < b < c` to `a < b and b < c` without calculating b twice
                        error pos [ "NI compops expansion" ]

                Op.Logical:
                    if notAllSeparators (x: x == firstSep) secondTail then
                        error pos [ "Mixing `and` and `or` is ambiguous. Use parens!" ]

                    else
                        translateBinopSepList_rightAssociative env pos firstItem firstTail

                Op.Tuple:
                    if thirdTail /= [] then
                        error pos [ "Tuples can't have more than 3 items, use a record instead." ]

                    else
                         translateExpression env firstItem >> onOk first:
                         translateExpression env secondItem >> onOk second:
                         translateExpression env thirdItem >> onOk third:
                         Dict.empty
                             >> Dict.insert "first" first
                             >> Dict.insert "second" second
                             >> Dict.insert "third" third
                             >> CA.Record pos Nothing
                             >> Ok

                Op.Pipe:
                    if notAllSeparators (x: x == firstSep) secondTail then
                        error pos [ "Mixing pipes is ambigous. Use parens." ]

                    else if firstSep.associativity == Op.Right then
                        translateBinopSepList_rightAssociative env pos firstItem firstTail

                    else
                        translateBinopSepList_leftAssociative env pos firstItem firstTail

                Op.Mutop:
                    error pos [ "mutops can't be chained" ]

                _:
                    translateBinopSepList_rightAssociative env pos firstItem firstTail


notAllSeparators as (sep: Bool): [ sep & item]: Bool =
    f: ls:
    try ls as
        []:
            False

        (sep & item) :: tail:
            if f sep then
                notAllSeparators f tail

            else
                True


sameDirectionAs as Op.Binop: Op.Binop: Bool =
    a: b:
    if a.symbol == b.symbol then
        True

    else
        try a.symbol as
            ">":
                b.symbol == ">="

            ">=":
                b.symbol == ">"

            "<":
                b.symbol == "<="

            "<=":
                b.symbol == "<"

            _:
                False


translateBinopSepList_rightAssociative as Env: Pos: FA.Expression: [ Op.Binop & FA.Expression ]: Res CA.Expression =
    env: pos: left: opsAndRight:
    translateExpression env left >> onOk caLeft:
    try opsAndRight as
        []:
            Ok caLeft

        (op & right) :: tail:
            translateBinopSepList_rightAssociative env pos right tail
            >> onOk caRight:

            makeBinop pos (CA.ArgumentExpression caLeft) op (CA.ArgumentExpression caRight)


translateBinopSepList_leftAssociative as Env: Pos: FA.Expression: [ Op.Binop & FA.Expression ]: Res CA.Expression =
    env: pos: leftAccum: opsAndRight:

    translateExpression env leftAccum >> onOk caLeftAccum:
    translateBinopSepListRec env pos caLeftAccum opsAndRight


translateBinopSepListRec as Env: Pos: CA.Expression: [ Op.Binop & FA.Expression ]: Res CA.Expression =
    env: pos: leftAccum: opsAndRight:
    try opsAndRight as
        []:
            Ok leftAccum

        (op & faRight) :: tail:
            translateArgument env faRight >> onOk caRight:
            makeBinop pos (CA.ArgumentExpression leftAccum) op caRight
            >> onOk binop:
            translateBinopSepListRec env pos binop tail


[# Unlike other ML languages, the left operand is the _second_ argument

`a + b` == `((+) b) a`

#]
makeBinop as Pos: CA.Argument: Op.Binop: CA.Argument: Res CA.Expression =
    pos: left: op: right:

    if op.symbol == Prelude.sendRight.symbol then
        # arg3 >> fun arg1 arg2
        try right as
            CA.ArgumentExpression (CA.Call p ref args):
                CA.Call p ref (List.concat [args, [left]])
                >> Ok

            CA.ArgumentExpression ref:
                CA.Call pos ref [left]
                >> Ok

            CA.ArgumentRecycle _ _ _:
                error pos [ "Can't >> to a recyclable" ]

    else if op.symbol == Prelude.sendLeft.symbol then
        # arg3 >> fun arg1 arg2
        try left as
            CA.ArgumentExpression (CA.Call p ref args):
                CA.Call p ref (List.concat [args, [right]])
                >> Ok

            CA.ArgumentExpression ref:
                CA.Call pos ref [right]
                >> Ok

            CA.ArgumentRecycle _ _ _:
                error pos [ "Can't << to a recyclable" ]

    else
        CA.Call pos (CA.Variable pos (RefGlobal op.usr)) [left, right]
        >> Ok


translateSimpleBinop as Env: Pos: FA.Expression: Op.Binop: FA.Expression: Res CA.Expression =
    env: pos: left: op: right:
    translateArgument env left >> onOk l:
    translateArgument env right >> onOk r:
    makeBinop pos l op r


#
# Type
#


translateAndInsertRecordAttributeType as ReadOnly: FA.RecordAttribute: Dict Name CA.Type: Res (Dict Name CA.Type) =
    ro: faAttr: caAttrs:

    translateAttributeName faAttr.name
    >> onOk (pos & name & maybeFaType):

    if Dict.member name caAttrs then
        error pos [ "Duplicate attribute name: " .. name ]
    else
        try maybeFaType as
            Nothing:
                error pos [ "I need to see the type of this attribute, `" .. name .. " as TheType`" ]

            Just faType:
                faType
                >> translateType ro
                >> onOk caType:

                if faAttr.maybeExpr /= Nothing then
                    error pos [ "I'm expecting a type here; `=` is for assignign values" ]
                else
                    caAttrs
                    >> Dict.insert name caType
                    >> Ok


translateNamedType as ReadOnly: Pos: Token.Word: [CA.Type]: Res CA.Type =
    ro: pos: word: caArgs:

    if word.modifier /= Token.NameNoModifier then
        error pos [ "I was expecting a type name here =|" ]
    else if word.attrPath /= [] then
        error pos [ "Type names have no attributes to access" ]
    else
        caArgs
        >> CA.TypeNamed (resolveToTypeUsr ro word.maybeModule word.name) (CA.UniIsFixed Imm)
        >> CA.Type pos
        >> Ok


translateTypeVariable as Pos: Token.Word: Res CA.Type =
    pos: word:

    if word.modifier /= Token.NameNoModifier then
        error pos [ "I was expecting a type variable name here =|" ]
    else if word.attrPath /= [] then
        error pos [ "Type variables have no attributes to access" ]
    else if word.maybeModule /= Nothing then
        error pos [ "No point it getting tyvars from modules?" ]
    else
        word.name
        >> CA.TypeAnnotationVariable Imm
        >> CA.Type pos
        >> Ok


translateTypeFunctionParameter as ReadOnly: FA.Expression: Res (RecycleOrSpend & CA.Type) =
    ro: expression:

    FA.Expression _ expr_ =
        expression

    try expr_ as
        FA.Unop Op.UnopRecycle faOperand:
            faOperand
            >> translateType ro
            >> onOk (CA.Type pos type_):

            typeSetUni pos Uni (CA.Type pos type_)
            >> onOk t:

            Recycle & t
            >> Ok

        _:
            expression
            >> translateType ro
            >> onOk caType:

            Spend & caType
            >> Ok


translateType as ReadOnly: FA.Expression: Res CA.Type =
    ro: (FA.Expression pos expr_):

    try expr_ as
        FA.Variable { maybeType, word }:
            if maybeType /= Nothing then
                error pos [ "Can't really specify the type of a type." ]
            else if word.isUpper then
                translateNamedType ro pos word []
            else
                translateTypeVariable pos word

        FA.Call (FA.Expression refPos ref) faArgs:
            try ref as
                FA.Variable { maybeType = Nothing, word }:
                    faArgs
                    >> List.mapRes (translateType ro)
                    >> onOk caArgs:

                    translateNamedType ro refPos word caArgs

                _:
                    error refPos [ "I was expecting a named type here" ]

        FA.List dotsAndItems:
            try dotsAndItems as
                []:
                    error pos [ "You need to specify the type of the List items" ]

                [ hasDots & faItem ]:
                    if hasDots then
                        error pos [ "No need to use dots here" ]
                    else
                        translateType ro faItem
                        >> onOk caItem:

                        CoreTypes.list caItem
                        >> Ok

                _:
                    error pos [ "List items must all have the same type, so you can specify only one type" ]

        FA.Record { maybeExtension, attrs }:
            if maybeExtension /= Nothing then
                error pos [ "Experimentally, extensible type annotations are disabled" ]

            else
                Dict.empty
                >> List.forRes attrs (translateAndInsertRecordAttributeType ro)
                >> onOk caAttrs:

                caAttrs
                >> CA.TypeRecord Imm
                >> CA.Type pos
                >> Ok

        FA.Fn faParams faReturn:
            faParams
            >> List.mapRes (translateTypeFunctionParameter ro)
            >> onOk caParams:

            faReturn
            >> translateType ro
            >> onOk caReturn:

            CA.TypeFn caParams caReturn
            >> CA.Type pos
            >> Ok

        FA.Binop Op.Tuple sepList:
            sepList
            >> translateTuple (translateType ro)
            >> onOk recordAttrs:

            CA.TypeRecord Imm recordAttrs
            >> CA.Type pos
            >> Ok

        FA.Unop Op.UnopUnique faOperand:
            faOperand
            >> translateType ro
            >> onOk t:

            typeSetUni pos Uni t

        _:
            # TODO: do all other constructors explicitly
            error pos [ "Not sure what's up with this type =|", toHuman expr_ ]


#
# Union constructor
#


translateConstructor as ReadOnly: CA.Type: USR: FA.Expression: Dict Name CA.Constructor: Res (Dict Name CA.Constructor) =
    ro: unionType: unionUsr: (FA.Expression pos expr_): constructors:

    zzz =
        try expr_ as
            FA.Variable var:
                var & [] >> Ok

            FA.Call (FA.Expression _ (FA.Variable var)) pars:
                var & pars >> Ok

            _:
                error pos [ "I was expecting a constructor name here" ]

    zzz
    >> onOk ({ maybeType, word } & faPars):

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
        >> List.mapRes (translateType ro)
        >> onOk caPars:

        c as CA.Constructor =
            {
            , pos
            , typeUsr = unionUsr
            , pars = caPars
            , type =
                if caPars == [] then
                    unionType
                else
                    CA.Type pos << CA.TypeFn (List.map (a: Spend & a) caPars) unionType
            }

        constructors
        >> Dict.insert word.name c
        >> Ok


translateTypeParameter as At Token.Word: Res (At Name) =
    (At pos word):

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


translateTypeName as At Token.Word: Res Name =
    (At pos word):

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


insertRootStatement as ReadOnly:  FA.Statement: CA.Module: Res (CA.Module) =
    ro: faStatement: caModule:

    try faStatement as
        FA.Evaluation (FA.Expression pos _):
            error pos [ "Root Evaluations don't really do much =|" ]

        FA.ValueDef def:
            def
            >> translateDefinition True (initEnv ro)
            >> onOk def:

            if CA.patternContainsUnique def.pattern then
                error (CA.patternPos def.pattern) [ "Mutable values can be declared only inside functions." ]


            # TODO check duplicates!!!!

            else
                # Patterns contain position, so they are unique and don't need to be checked for duplication
                # Names duplication will be checked when rootValuesAndConstructors is populated
                Ok { caModule with valueDefs = Dict.insert def.pattern def .valueDefs }

        FA.AliasDef fa:
            translateTypeName fa.name
            >> onOk name:

            if Dict.member name caModule.aliasDefs or Dict.member name caModule.unionDefs then
                At pos _ = fa.name
                error pos [ name .. " declared twice!" ]

            else
                fa.args
                >> List.mapRes translateTypeParameter
                >> onOk caPars:

                # TODO check that args are not duplicate

                fa.type
                >> translateType ro
                >> onOk type:

                aliasDef as CA.AliasDef =
                    {
                    , usr = USR ro.currentModule name
                    , pars = caPars
                    , type
                    , directTypeDeps = typeDeps type Set.empty
                    }

                Ok { caModule with aliasDefs = Dict.insert name aliasDef .aliasDefs }

        FA.UnionDef fa:
            At pos _ = fa.name

            translateTypeName fa.name
            >> onOk name:

            if Dict.member name caModule.aliasDefs or Dict.member name caModule.unionDefs then
                error pos [ name .. " declared twice!" ]

            else
                fa.args
                >> List.mapRes translateTypeParameter
                >> onOk caPars:

                # TODO check that args are not duplicate

                usr =
                    USR ro.currentModule name

                type =
                    caPars
                    >> List.map ((At p name): CA.Type p (CA.TypeAnnotationVariable Imm name))
                    >> CA.TypeNamed usr (CA.UniIsFixed Imm)
                    >> CA.Type pos

                Dict.empty
                >> List.forRes fa.constructors (translateConstructor ro type usr)
                >> onOk constructors:

                unionDef as CA.UnionDef =
                    {
                    , usr
                    , pars = caPars
                    , constructors
                    # I could probably break the deps by constructor, but would it be much useful in practice?
                    , directTypeDeps = Dict.for constructors (k: c: List.for c.pars typeDeps) Set.empty
                    }

                Ok { caModule with unionDefs = Dict.insert name unionDef .unionDefs }


translateModule as ReadOnly: Text: UMR: FA.Module: Res (CA.Module) =
    ro: asText: umr: faModule:

    Debug.benchStart None

    module =
        CA.initModule asText umr

    # Add all definitions
    module
    >> List.forRes faModule (insertRootStatement ro)
    >> btw Debug.benchStop "translateModule"


textToCanonicalModule as Params: Text: Res CA.Module =
    pars: code:

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
    >> Result.onOk faModule:
    translateModule ro code umr faModule

