
alias Params = {
    , meta as Meta
    , stripLocations as Bool
    , source as Meta.Source
    , name as Name
    }


textToCanonicalModule as Params: Text: Res CA.Module =
    pars: code:

    ro as Compiler/MakeCanonical.ReadOnly = {
        , currentModule = Meta.UMR pars.source pars.name
        , meta = pars.meta
        }

    umr =
        Meta.UMR pars.source pars.name

    code
        >> Compiler/Parser.textToFormattableModule {
            , stripLocations = pars.stripLocations
            , moduleName = pars.name
            }
        >> Result.onOk (x: todo "translateModule ro code umr")



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
    , currentModule as Meta.UniqueModuleReference
    , meta as Meta
    }


initEnv as ReadOnly: Env =
    ro: {
    , maybeShorthandTarget = Nothing
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
# Names resolution
#

maybeForeignUsr as (Meta: Dict Text Meta.UniqueSymbolReference): ReadOnly: Maybe Name: Name: Maybe Meta.UniqueSymbolReference =
    getter: ro: maybeModule: name:

    try maybeModule as
        Just moduleName:
            try Dict.get moduleName ro.meta.moduleVisibleAsToUmr as
                Just umr:
                    Just << Meta.USR umr name

                Nothing:
                    # TODO should this produce an error instead?
                    # i.e., does ro.meta.moduleVisibleAsToUmr contain *all* modules, aliased or not?
                    #CA.RefGlobal (Meta.USR Meta.SourcePlaceholder moduleName name)
#                    List.each (Dict.keys ro.meta.moduleVisibleAsToUmr) x:
#                        log "*" x
                    todo << "!!resolveToUsr can't find the module: " .. moduleName .. " (for: " .. name .. ")"

        Nothing:
            Dict.get name (getter ro.meta)


resolveToUsr as (Meta: Dict Text Meta.UniqueSymbolReference): ReadOnly: Maybe Name: Name: Meta.UniqueSymbolReference =
    getter: ro: maybeModule: name:

    maybeForeignUsr getter ro maybeModule name
        >> Maybe.withDefault (Meta.USR ro.currentModule name)


resolveToValueRef as ReadOnly: Bool: Maybe Name: Name: CA.Ref =
    ro: declaredInsideFunction: maybeModule: name:

    try maybeForeignUsr (m: m.globalValues) ro maybeModule name as
        Just usr:
            CA.RefGlobal usr

        Nothing:
            if declaredInsideFunction then
                CA.RefLocal name

            else
                CA.RefGlobal << Meta.USR ro.currentModule name


resolveToTypeUsr as ReadOnly: Maybe Name: Name: Meta.UniqueSymbolReference =
    resolveToUsr (m: m.globalTypes)


resolveToConstructorUsr as ReadOnly: Maybe Name: Name: Meta.UniqueSymbolReference =
    resolveToUsr (m: m.globalValues)


#
# Dependencies
#


typeDeps as CA.Type: Set Meta.UniqueSymbolReference: Set Meta.UniqueSymbolReference =
    type: acc:
    try type as
        CA.TypeNamed _ usr args: acc >> Set.insert usr >> List.for args typeDeps
        CA.TypeAnnotationVariable _ _: acc
        CA.TypeFunction _ params to: acc >> typeDeps to >> List.for params ((_ & f): typeDeps f)
        CA.TypeRecord _ attrs: Dict.for attrs (k: typeDeps) acc
        CA.TypeUnique _ t: typeDeps t acc


alias Deps = {
    , types as Set Meta.UniqueSymbolReference
    , cons as Set Meta.UniqueSymbolReference
    , values as Set Meta.UniqueSymbolReference
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

        CA.Variable _ (CA.RefGlobal usr):
            { deps with values = Set.insert usr .values }

        CA.Variable _ _:
            deps

        CA.Constructor _ usr:
            { deps with cons = Set.insert usr .cons }

        CA.Lambda _ pa isConsuming body:
            deps
                >> patternDeps pa
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

        CA.Call _ e0 a:
            deps
                >> expressionDeps e0
                >> argumentDeps a

        CA.CallCo _ e0 args:
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


#
# Definition
#


translateDefinition as Bool: Env: FA.ValueDef: Res (CA.ValueDef) =
    isRoot: env: fa:

    translatePattern env fa.pattern
    >> onOk pattern:

    tyvars as Dict Name CA.TypeClasses =
        todo "xxxx"
#        pattern
#        >> CA.patternTyvars
#        >> Dict.map tyvarName: pos:
#            {
#            , allowFunctions = Just << not << List.member tyvarName fa.nonFn
#            , allowUniques = Just False
#            }

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

    Ok {
        , pattern
        , tyvars
        , native = False
        , body
        #
        , directTypeDeps = deps.types
        , directConsDeps = deps.cons
        , directValueDeps = deps.values
        }




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


translatePatternAny as Env: Pos: Maybe FA.Expression: Token.Word: Res CA.Pattern =
    env: pos: maybeFaType: word:

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

        # TODO isUnique
        Ok << CA.PatternAny pos { isUnique = False, maybeName, maybeAnnotation = maybeCaType }


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
            error pos [ "tuples can be only of size 2 or 3" ]



translatePattern as Env: FA.Expression: Res CA.Pattern =
    env: (FA.Expression pos expr_):

    try expr_ as

        FA.Variable { maybeType, word }:

            if word.isUpper then
                if maybeType /= Nothing then
                    error pos [ "Pattern constructors can't have type annotations" ]
                else
                    translatePatternConstructor env pos word []
            else
                translatePatternAny env pos maybeType word

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
                        >> onOk caArgs:
                        translatePatternConstructor env pos word caArgs

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

                                translatePatternAny env pos maybeType word
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
    [#
translateStatementBlock as Env: [FA.Statement]: Res CA.Expression =
    env: stats:

    todo "translateStatementBlock"
    try stats as
        FA.TypeAlias fa :: _:
            At pos _ = fa.name
            error pos [ "Aliases can be declared only in the root scope" ]

        FA.UnionDef pos fa :: _:
            error pos [ "Types can be declared only in the root scope" ]

        []:
            Ok << CA.Constructor Pos.G CoreTypes.noneValue

        # Last evaluation in a block is the return statement
        FA.Evaluation pos faExpr :: []:
            translateExpression env faExpr >> onOk e:
            Ok << e

        # Non-last evaluation gets converted into a definition and then into a LetIn
        FA.Evaluation pos faExpr :: tail:
            # TODO Non-return, non-mutable, non-debug evaluations should produce an error.
            valueDef as FA.ValueDef = {
              , pattern = FA.PatternAny Pos.G False "_" Nothing
              , nonFn = []
              , body = [ FA.Evaluation pos faExpr ]
              }

            translateDefinition False env valueDef >> onOk d:
            translateStatementBlock env tail >> onOk tailBlockExpression:
            Ok << CA.LetIn d tailBlockExpression

        FA.Definition pos fa :: tail:
            translateDefinition False env fa >> onOk d:
            translateStatementBlock { env with nonRootValues = Dict.join (CA.patternNames d.pattern) .nonRootValues } tail >> onOk tailBlockExpression:
            Ok << CA.LetIn d tailBlockExpression
    #]



##
#- Expression
#
translateExpression as Env: FA.Expression: Res CA.Expression =
    env: faExpr:

    todo "translateExpression"
    [#
    try faExpr as
        FA.LiteralNumber pos str:
            translateNumber CA.LiteralNumber pos str

        FA.LiteralText pos v:
            Ok << CA.LiteralText pos v

        FA.PrefixBinop pos symbol:
            CoreTypes.makeUsr symbol
            >> CA.RefGlobal
            >> CA.Variable pos
            >> Ok

        FA.Variable pos maybeModule name attrs:
            declaredInsideFunction =
                Dict.member name env.nonRootValues

            resolveToValueRef env.ro declaredInsideFunction maybeModule name
            >> CA.Variable pos
            >> List.for attrs (CA.RecordAccess pos)
            >> Ok

        FA.Constructor pos maybeModule name:
            resolveToConstructorUsr env.ro maybeModule name
                >> CA.Constructor pos
                >> Ok

        FA.Mutable pos _ _:
            error pos [ "can't use mutability here?" ]

        FA.RecordShorthand pos attrPath:
            try env.maybeShorthandTarget as
                Nothing:
                    error pos [
                        , "Record update shorthands must be used inside a record update such as"
                        , "    { aRecord with anAttribute = doSomethingWith ." .. Text.join "." attrPath .. " }"
                        , "but we are not inside a record update!"
                        ]

                Just shorthandTarget:
                    shorthandTarget
                    >> List.for attrPath (attrName: expr: CA.RecordAccess pos attrName expr)
                    >> Ok

        FA.Lambda pos faParam isConsuming faBody:

            translatePattern env faParam
            >> onOk caPattern:

            localEnv =
                { env with
                , nonRootValues =
                    caPattern
                    >> CA.patternNames
                    >> Dict.join .nonRootValues
                }

            translateStatementBlock localEnv faBody
            >> onOk caBody:

            Ok << CA.Lambda pos caPattern isConsuming caBody


        # TODO: this is temporary, can be removed once we remove auto-currying
        FA.FunctionCall callPos (FA.Constructor consPos maybeModule name) arguments:

            caReference =
                resolveToConstructorUsr env.ro maybeModule name
                >> CA.Constructor consPos

            List.mapRes (translateArgument env) arguments
            >> onOk args:

            CA.CallCo callPos caReference args
            >> Ok


        FA.FunctionCall pos reference arguments:
            # ref arg1 arg2 arg3...
            fold as CA.Argument: CA.Expression: CA.Expression =
                argument: refAccum:
                CA.Call pos refAccum argument

            translateExpression env reference >> onOk ref:
            List.mapRes (translateArgument env) arguments >> onOk args:
            Ok << List.for args fold ref

        FA.If pos { condition, true, false, isCompact }:
            translateExpression env condition >> onOk c:
            translateStatementBlock env true >> onOk t:
            translateStatementBlock env false >> onOk f:
            { condition = c
            , true = t
            , false = f
            }
                >> CA.If pos
                >> Ok

        FA.Unop pos op faOperand:
            translateExpression env faOperand >> onOk caOperand:
            CA.Call pos
                (CA.Variable pos (CA.RefGlobal op.usr))
                (CA.ArgumentExpression caOperand)
            >> Ok

        FA.Binop pos group sepList:
            translateBinops env pos group sepList

        FA.Record pos faArgs:
            makeUpdateTarget pos env faArgs.extends
            >> onOk caUpdateTarget:

            # TODO use a Result.list_fold?
            translateAttrsRec { env with maybeShorthandTarget = caUpdateTarget.maybeExpr } faArgs.attrs Dict.empty >> onOk caAttrs:
            caAttrs
                >> CA.Record pos caUpdateTarget.maybeExpr
                >> caUpdateTarget.wrapper
                >> Ok

        FA.List pos faItems:
            cons = item: list:
                CA.Call pos
                    (CA.Call pos
                        (CA.Constructor pos CoreTypes.cons)
                        (CA.ArgumentExpression item)
                    )
                    (CA.ArgumentExpression list)

            List.mapRes (translateExpression env) faItems >> onOk es:
            Ok << List.forReversed es cons (CA.Constructor pos CoreTypes.nil)

        FA.Try pos fa:
            translatePatternAndStatements = ( faPattern & faStatements ):
                translatePattern env faPattern >> onOk caPattern:
                translateStatementBlock { env with nonRootValues = Dict.join (CA.patternNames caPattern) env.nonRootValues } faStatements >> onOk block:
                Ok ( caPattern & block )

            translateExpression env fa.value >> onOk caValue:
            List.mapRes translatePatternAndStatements fa.patterns >> onOk caPatternsAndStatements:
            Ok << CA.Try pos { value = caValue, patternsAndExpressions = caPatternsAndStatements }

      #]


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


makeUpdateTarget as Pos: Env: Maybe FA.Expression: Res { maybeExpr as Maybe CA.Expression, wrapper as CA.Expression: CA.Expression } =
    pos: env: maybeShorthandTarget:

    try maybeShorthandTarget as
        Nothing:
            Ok { maybeExpr = Nothing, wrapper = identity }

        Just shorthand:
            translateExpression { env with maybeShorthandTarget = Nothing } shorthand
            >> onOk expression:

            try expression as
                CA.Variable _ ref: Ok { maybeExpr = Just expression, wrapper = identity }
                _: error pos [ "NI { (expr) with ...} not yet implemented =(" ]



translateAttrsRec as Env: [(At Text) & Maybe FA.Expression]: Dict Text CA.Expression: Res (Dict Text CA.Expression) =
    env: faAttrs: caAttrsAccum:
    try faAttrs as
        []:
            Ok caAttrsAccum

        ((At pos attrName) & maybeAttrExpression) :: faTail:
            exprRes =
                try maybeAttrExpression as
                    Just faExpr:
                        translateExpression env faExpr

                    Nothing:
                        declaredInsideFunction =
                            Dict.member attrName env.nonRootValues

                        attrName
                        >> resolveToValueRef env.ro declaredInsideFunction Nothing
                        >> CA.Variable pos
                        >> Ok

            exprRes >> onOk expr:
            translateAttrsRec env faTail (Dict.insert attrName expr caAttrsAccum)


translateArgument as Env: FA.Expression: Res (CA.Argument) =
    env: faExpr:

    todo "translateArgument"
#    try faExpr as
#        FA.Mutable pos name attrPath:
#            if Dict.member name env.nonRootValues then
#                CA.RefLocal name
#                >> CA.ArgumentRecycle pos attrPath
#                >> Ok
#
#            else
#                error pos
#                    [ "only values declared inside a function scope can be mutated!"
#                    ]
#
#        _:
#            faExpr
#                >> translateExpression env
#                >> Result.map CA.ArgumentExpression


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
            translateBinopSepList_rightAssociative env pos right tail >> onOk caRight:
            Ok << makeBinop pos (CA.ArgumentExpression caLeft) op (CA.ArgumentExpression caRight)


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
            translateBinopSepListRec env pos (makeBinop pos (CA.ArgumentExpression leftAccum) op caRight) tail


[# Unlike other ML languages, the left operand is the _second_ argument

`a + b` == `((+) b) a`

#]
makeBinop as Pos: CA.Argument: Op.Binop: CA.Argument: CA.Expression =
    pos: left: op: right:
    try left & op.symbol & right as

        # TODO don't hardcode the strings, use instead those defined in Prelude
        _ & ">>" & CA.ArgumentExpression rightExpr :
            CA.Call pos rightExpr left

        CA.ArgumentExpression leftExpr & "<<" & _:
            CA.Call pos leftExpr right

        _:
            CA.Call pos
                (CA.Call pos
                    (CA.Variable pos
                        (CA.RefGlobal op.usr)
                    )
                    right
                )
                left


translateSimpleBinop as Env: Pos: FA.Expression: Op.Binop: FA.Expression: Res CA.Expression =
    env: pos: left: op: right:
    translateArgument env left >> onOk l:
    translateArgument env right >> onOk r:
    Ok << makeBinop pos l op r


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
        >> CA.TypeNamed pos (resolveToTypeUsr ro word.maybeModule word.name)
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
        >> CA.TypeAnnotationVariable pos
        >> Ok


translateType as ReadOnly: FA.Expression: Res CA.Type =
    ro: (FA.Expression pos expr_):

    try expr_ as
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
                >> CA.TypeRecord pos
                >> Ok

        FA.Variable { maybeType, word }:
            if maybeType /= Nothing then
                error pos [ "Can't really specify the type of a type." ]
            else if word.isUpper then
                translateNamedType ro pos word []
            else
                translateTypeVariable pos word

        FA.Fn faParams faReturn:
            faParams
            >> List.mapRes (translateType ro)
            >> onOk caParams:

            faReturn
            >> translateType ro
            >> onOk caReturn:

            CA.TypeFunction pos (List.map (t: LambdaNormal & t) caParams) caReturn
            >> Ok

        FA.Call (FA.Expression refPos ref) faArgs:
            try ref as
                FA.Variable { maybeType = Nothing, word }:
                    faArgs
                    >> List.mapRes (translateType ro)
                    >> onOk caArgs:

                    translateNamedType ro refPos word caArgs

                _:
                    error refPos [ "I was expecting a named type here" ]

        FA.Binop Op.Tuple sepList:
            sepList
            >> translateTuple (translateType ro)
            >> onOk recordAttrs:

            CA.TypeRecord pos recordAttrs
            >> Ok

        _:
            # TODO: do all other constructors
            error pos [ "Not sure what's up with this type =|" ]


#
# Union constructor
#


translateConstructor as ReadOnly: CA.Type: Meta.UniqueSymbolReference: FA.Expression: Dict Name CA.Constructor: Res (Dict Name CA.Constructor) =
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

        c as CA.Constructor = {
            , pos
            , typeUsr = unionUsr
            , type = CA.TypeFunction pos (List.map (a: LambdaConsuming & a) caPars) unionType
            , args = caPars
            }

        constructors
        >> Dict.insert word.name c
        >> Ok


#
# Module
#


insertRootStatement as ReadOnly: FA.Statement: CA.Module: Res (CA.Module) =
    ro: faStatement: caModule:

    try faStatement as
        FA.Evaluation (FA.Expression pos _):
            error pos [ "Root Evaluations don't really do much =|" ]

        FA.ValueDef fa:
            translateDefinition True (initEnv ro) fa
            >> onOk def:


            if CA.patternContainsUnique def.pattern then
                error (CA.patternPos def.pattern) [ "Mutable values can be declared only inside functions." ]

            else

                # Patterns contain position, so they are unique and don't need to be checked for duplication
                # Names duplication will be checked when rootValuesAndConstructors is populated
                Ok { caModule with valueDefs = Dict.insert def.pattern def .valueDefs }

        FA.AliasDef fa:
            todo "AliasDef"
#            At pos name =
#                fa.name
#
#            if Dict.member name caModule.aliasDefs or Dict.member name caModule.unionDefs then
#                error pos [ name .. " declared twice!" ]
#
#            else
#                # TODO check args!
#                translateType ro fa.ty >> onOk type:
#
#                aliasDef as CA.AliasDef = {
#                    , usr = Meta.USR ro.currentModule (Pos.drop fa.name)
#                    , args = fa.args
#                    , type
#                    , directTypeDeps = typeDeps type Set.empty
#                    }
#
#                Ok { caModule with aliasDefs = Dict.insert name aliasDef .aliasDefs }

        FA.UnionDef fa:
            todo "UnionDef"
#            if Dict.member fa.name caModule.aliasDefs or Dict.member fa.name caModule.unionDefs then
#                error pos [ fa.name .. " declared twice!" ]
#
#            else
#                usr =
#                    Meta.USR ro.currentModule fa.name
#
#                type =
#                    fa.args
#                    >> List.map (name: CA.TypeAnnotationVariable pos name)
#                    >> CA.TypeNamed pos usr
#
#                Dict.empty
#                >> List.forRes fa.constructors (translateConstructor ro type usr)
#                >> onOk constructors:
#
#                unionDef as CA.UnionDef = {
#                    , usr
#                    , args = fa.args
#                    , constructors
#                    # I could probably break the deps by constructor, but would it be much useful in practice?
#                    , directTypeDeps = Dict.for constructors (k: c: List.for c.args typeDeps) Set.empty
#                    }
#
#                Ok { caModule with unionDefs = Dict.insert fa.name unionDef .unionDefs }



translateModule as ReadOnly: Text: Meta.UniqueModuleReference: FA.Module: Res (CA.Module) =
    ro: asText: umr: faModule:

#    todo "translateModule"
    Debug.benchStart None

    module =
        CA.initModule asText umr

    # Add all definitions
    module
    >> List.forRes faModule (insertRootStatement ro)
    >> btw Debug.benchStop "translateModule"

