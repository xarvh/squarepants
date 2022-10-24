
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
            , name = pars.name
            }
        >> Result.onOk (translateModule ro code umr)


alias Constructor = CA.Constructor
alias Expression = CA.Expression
alias Pattern = CA.Pattern

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
    , maybeShorthandTarget as Maybe Expression
    #
    # This keeps track of values that are declared within the
    # scope of a function, so they don't need to be expanded with the module name.
    #
    , nonRootValues as Dict Text Pos
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


makeError as Pos: [Text]: Res a =
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


typeDeps as CA.CanonicalType: Set Meta.UniqueSymbolReference: Set Meta.UniqueSymbolReference =
    type: acc:
    try type as
        CA.TypeOpaque _ usr args: acc >> Set.insert usr >> List.for args typeDeps
        CA.TypeAnnotationVariable _ _: acc
        CA.TypeFunction _ from _ to: acc >> typeDeps from >> typeDeps to
        CA.TypeRecord _ attrs: Dict.for attrs (k: typeDeps) acc
        #CA.TypeRecordExt _ attrs: Dict.for attrs (k: typeDeps) acc
        CA.TypeUnique _ t: typeDeps t acc
        CA.TypeAlias _ _ _: todo "typeDeps: Should not happen"


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


patternDeps as Pattern: Deps: Deps =
    pattern: deps:

    try pattern as

        CA.PatternConstructor _ usr ps:
            List.for ps patternDeps { deps with cons = Set.insert usr .cons }

        CA.PatternRecord _ ps:
            Dict.for ps (k: patternDeps) deps

        CA.PatternAny _ { isUnique = _, maybeName = _, maybeAnnotation = Just type }:
            { deps with types = typeDeps type .types }

        CA.PatternAny _ { isUnique = _, maybeName = _, maybeAnnotation = Nothing }:
            deps

        CA.PatternLiteralNumber _ _:
           deps

        CA.PatternLiteralText _ _:
           deps


expressionDeps as Expression: Deps: Deps =
    expr: deps:
    try expr as
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
        pattern
        >> CA.patternTyvars
        >> Dict.map tyvarName: pos:
            {
            , allowFunctions = Just << not << List.member tyvarName fa.nonFn
            , allowUniques = Just False
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

    translateStatementBlock localEnv0 fa.body
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


translatePattern as Env: FA.Pattern: Res Pattern =
    env: fa:
    try fa as
        FA.PatternAny pos isMutable name maybeFaType:

            getMaybeCaType as Res (Maybe CA.CanonicalType) =
                try maybeFaType as
                    Just faType:
                        faType
                        >> translateType False env.ro
                        >> Result.map Just

                    _:
                        Ok Nothing

            getMaybeCaType
            >> onOk maybeCaType:

            maybeName =
                if name == "_" then Nothing else (Just name)

            Ok << CA.PatternAny pos { isUnique = isMutable, maybeName, maybeAnnotation = maybeCaType }

        FA.PatternLiteralNumber pos l:
            translateNumber CA.PatternLiteralNumber pos l

        FA.PatternLiteralText pos l:
            Ok << CA.PatternLiteralText pos l

        FA.PatternConstructor pos maybeModule name faArgs:
            List.mapRes (translatePattern env) faArgs >> onOk caArgs:
            Ok << CA.PatternConstructor pos (resolveToConstructorUsr env.ro maybeModule name) caArgs

        FA.PatternList pos fas:
            fold = pattern: last:
                # TODO pos is probably inaccurate
                CA.PatternConstructor pos CoreTypes.cons [ pattern, last ]

            List.mapRes (translatePattern env) fas >> onOk cas:
            Ok << List.forReversed cas fold (CA.PatternConstructor pos CoreTypes.nil [])

        FA.PatternRecord pos recordArgs:
            if recordArgs.extends /= Nothing then
                makeError pos [ "can't use `with` inside patterns" ]

            else
                fold =
                    ( (At p name) & maybePattern ): dict:

                    if Dict.member name dict then
                        makeError p [ "duplicate attribute name in pattern: " .. name ]

                    else
                        try maybePattern as
                            Nothing:
                                Ok << Dict.insert name (CA.PatternAny p { isUnique = False, maybeName = Just name, maybeAnnotation = Nothing }) dict

                            Just faPattern:
                                faPattern
                                    >> translatePattern env
                                    >> Result.map (caPattern: Dict.insert name caPattern dict)

                Dict.empty
                >> List.forRes recordArgs.attrs fold
                >> Result.map (x: x >> CA.PatternRecord pos)

        FA.PatternListCons pos pas:
            List.mapRes (translatePattern env) pas >> onOk caPas:
            try List.reverse caPas as
                last :: rest:
                     last
                        >> List.for rest (item: list: CA.PatternConstructor pos CoreTypes.cons [ item, list ])
                        >> Ok

                []:
                    makeError pos [ "should not happen: empty cons pattern" ]

        FA.PatternTuple pos fas:
            try fas as
                [ fa1, fa2 ]:
                    { extends = Nothing
                    , attrs =
                        [ (At (FA.patternPos fa1) "first") & Just fa1
                        , (At (FA.patternPos fa2) "second") & Just fa2
                        ]
                    }
                        >> FA.PatternRecord pos
                        >> translatePattern env

                [ fa1, fa2, fa3 ]:
                    { extends = Nothing
                    , attrs =
                        [ (At (FA.patternPos fa1) "first") & Just fa1
                        , (At (FA.patternPos fa2) "second") & Just fa2
                        , (At (FA.patternPos fa3) "third") & Just fa3
                        ]
                    }
                        >> FA.PatternRecord pos
                        >> translatePattern env

                _:
                    makeError pos [ "tuples can be only of size 2 or 3" ]


#
# Statement
#
translateStatementBlock as Env: [FA.Statement]: Res Expression =
    env: stats:

    try stats as
        FA.TypeAlias fa :: _:
            At pos _ = fa.name
            makeError pos [ "Aliases can be declared only in the root scope" ]

        FA.UnionDef pos fa :: _:
            makeError pos [ "Types can be declared only in the root scope" ]

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



##
#- Expression
#
translateExpression as Env: FA.Expression: Res Expression =
    env: faExpr:
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
            makeError pos [ "can't use mutability here?" ]

        FA.RecordShorthand pos attrPath:
            try env.maybeShorthandTarget as
                Nothing:
                    makeError pos [
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
            fold as CA.Argument: Expression: Expression =
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


translateNumber as (Pos: Number: a): Pos: Text: Res a =
    constructor: pos: numberAsText:

    try Text.toNumber numberAsText as
        Nothing:
            makeError pos [
                , "invalid number: `" .. numberAsText .. "`"
                , "TODO link to documentation on valid number formats"
                ]

        Just n:
            Ok << constructor pos n


makeUpdateTarget as Pos: Env: Maybe FA.Expression: Res { maybeExpr as Maybe Expression, wrapper as Expression: Expression } =
    pos: env: maybeShorthandTarget:

    try maybeShorthandTarget as
        Nothing:
            Ok { maybeExpr = Nothing, wrapper = identity }

        Just shorthand:
            translateExpression { env with maybeShorthandTarget = Nothing } shorthand
            >> onOk expression:

            try expression as
                CA.Variable _ ref: Ok { maybeExpr = Just expression, wrapper = identity }
                _: makeError pos [ "NI { (expr) with ...} not yet implemented =(" ]



translateAttrsRec as Env: [(At Text) & Maybe FA.Expression]: Dict Text Expression: Res (Dict Text Expression) =
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
    try faExpr as
        FA.Mutable pos name attrPath:
            if Dict.member name env.nonRootValues then
                CA.RefLocal name
                >> CA.ArgumentRecycle pos attrPath
                >> Ok

            else
                makeError pos
                    [ "only values declared inside a function scope can be mutated!"
                    ]

        _:
            faExpr
                >> translateExpression env
                >> Result.map CA.ArgumentExpression


translateBinops as Env: Pos: Op.Precedence: FA.SepList Op.Binop FA.Expression: Res Expression =
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
                        makeError pos [ "can't mix comparison ops with different direction" ]

                    else
                        # TODO expand `a < b < c` to `a < b and b < c` without calculating b twice
                        makeError pos [ "NI compops expansion" ]

                Op.Logical:
                    if notAllSeparators (x: x == firstSep) secondTail then
                        makeError pos [ "Mixing `and` and `or` is ambiguous. Use parens!" ]

                    else
                        translateBinopSepList_rightAssociative env pos firstItem firstTail

                Op.Tuple:
                    if thirdTail /= [] then
                        makeError pos [ "Tuples can't have more than 3 items, use a record instead." ]

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
                        makeError pos [ "Mixing pipes is ambigous. Use parens." ]

                    else if firstSep.associativity == Op.Right then
                        translateBinopSepList_rightAssociative env pos firstItem firstTail

                    else
                        translateBinopSepList_leftAssociative env pos firstItem firstTail

                Op.Mutop:
                    makeError pos [ "mutops can't be chained" ]

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


translateBinopSepList_rightAssociative as Env: Pos: FA.Expression: [ Op.Binop & FA.Expression ]: Res Expression =
    env: pos: left: opsAndRight:
    translateExpression env left >> onOk caLeft:
    try opsAndRight as
        []:
            Ok caLeft

        (op & right) :: tail:
            translateBinopSepList_rightAssociative env pos right tail >> onOk caRight:
            Ok << makeBinop pos (CA.ArgumentExpression caLeft) op (CA.ArgumentExpression caRight)


translateBinopSepList_leftAssociative as Env: Pos: FA.Expression: [ Op.Binop & FA.Expression ]: Res Expression =
    env: pos: leftAccum: opsAndRight:

    translateExpression env leftAccum >> onOk caLeftAccum:
    translateBinopSepListRec env pos caLeftAccum opsAndRight


translateBinopSepListRec as Env: Pos: Expression: [ Op.Binop & FA.Expression ]: Res Expression =
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
makeBinop as Pos: (CA.Argument): Op.Binop: (CA.Argument): Expression =
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


translateSimpleBinop as Env: Pos: FA.Expression: Op.Binop: FA.Expression: Res Expression =
    env: pos: left: op: right:
    translateArgument env left >> onOk l:
    translateArgument env right >> onOk r:
    Ok << makeBinop pos l op r



#
# Type
#
#union TranslateMode =
#    , ModeAlias
#    , ModeUnion
#    , ModeAnnotation {
#        # ensureUniqueName is used to ensure tyvar names are unique within the module
#        , ensureUniqueName as Pos: Name: Name
#        , nonFunctionByName as Set Name
#        }


addAttributes as Bool: ReadOnly: Pos: [ (At Text) & Maybe FA.Type ]: Dict Text CA.CanonicalType: Res CA.CanonicalType =
    allowUniques: ro: pos: faAttrs: caAttrsAccum:
    try faAttrs as
        []:
            CA.TypeRecord pos caAttrsAccum
            >> Ok

        ((At p name) & maybeFaType) :: faTail:
            try maybeFaType as
                Nothing:
                    makeError p [ "Attribute `" .. name .. "` must have a type" ]

                Just faType:
                    translateType allowUniques ro faType >> onOk caType:
                    addAttributes allowUniques ro p faTail (Dict.insert name caType caAttrsAccum)


translateType as Bool: ReadOnly: FA.Type: Res CA.CanonicalType =
    allowUniques: ro: faType:

    try faType as
        FA.TypeVariable pos name:
            CA.TypeAnnotationVariable pos name
            >> Ok


        FA.TypeConstant pos maybeModule name args:
            List.mapRes (translateType allowUniques ro) args >> onOk caArgs:
            caArgs
                >> CA.TypeOpaque pos (resolveToTypeUsr ro maybeModule name)
                >> Ok

        FA.TypeFunction pos fa_from lambdaModifier fa_to:
            translateType allowUniques ro fa_from >> onOk ca_from:
            translateType allowUniques ro fa_to >> onOk ca_to:
            Ok << CA.TypeFunction pos ca_from lambdaModifier ca_to

        FA.TypeTuple pos types:
            try types as
                [ faFirst, faSecond ]:
                    translateType allowUniques ro faFirst >> onOk caFirst:
                    translateType allowUniques ro faSecond >> onOk caSecond:
                    Dict.empty
                        >> Dict.insert "first" caFirst
                        >> Dict.insert "second" caSecond
                        >> CA.TypeRecord pos
                        >> Ok

                [ faFirst, faSecond, faThird ]:
                    translateType allowUniques ro faFirst >> onOk caFirst:
                    translateType allowUniques ro faSecond >> onOk caSecond:
                    translateType allowUniques ro faThird >> onOk caThird:
                    Dict.empty
                        >> Dict.insert "first" caFirst
                        >> Dict.insert "second" caSecond
                        >> Dict.insert "third" caThird
                        >> CA.TypeRecord pos
                        >> Ok

                _:
                    makeError pos [ "Tuples can only have size 2 or 3. Use a record." ]

        FA.TypeMutable pos t:
            translateType allowUniques ro t >> onOk cat:
            Ok << CA.TypeUnique pos cat

        FA.TypeList pos faItem:
            translateType allowUniques ro faItem >> onOk caItem:
            Ok << CoreTypes.list caItem

        FA.TypeRecord p recordArgs:
            if recordArgs.extends /= Nothing then
                makeError p ["For now extensible types are disabled, I want to see if it's good to do without them" ]

            else
                addAttributes allowUniques ro p recordArgs.attrs Dict.empty


#
# Union constructor
#


translateConstructor as ReadOnly: CA.CanonicalType: Meta.UniqueSymbolReference: At Name & [FA.Type]: Dict Name Constructor: Res (Dict Name Constructor) =
    ro: unionType: unionUsr: (At pos name & faArgs): constructors:

    if Dict.member name constructors then
        # TODO "union $whatever has two constructors with the same name!"
        makeError pos [ "constructor " .. name .. " is duplicate" ]

    else
        faArgs
        >> List.mapRes (translateType True ro)
        >> onOk caArgs:

        returnsUnique =
            List.any CA.typeContainsUniques caArgs

        t as CA.Type =
            if returnsUnique then
                CA.TypeUnique pos unionType
            else
                unionType

        c as CA.Constructor = {
            , pos
            , typeUsr = unionUsr
            , type = t >> List.forReversed caArgs (ar: ty: CA.TypeFunction pos ar LambdaConsuming ty)
            , args = caArgs
            }

        Ok << Dict.insert name c constructors


#
# Module
#


insertRootStatement as ReadOnly: FA.Statement: CA.Module: Res (CA.Module) =
    ro: faStatement: caModule:
    try faStatement as
        FA.Evaluation pos expr:
            makeError (FA.expressionPos expr) [ "Root Evaluations don't really do much =|" ]

        FA.Definition pos fa:
            translateDefinition True (initEnv ro) fa
            >> onOk def:


            if CA.patternContainsUnique def.pattern then
                makeError (CA.patternPos def.pattern) [ "Mutable values can be declared only inside functions." ]

            else

                # Patterns contain position, so they are unique and don't need to be checked for duplication
                # Names duplication will be checked when rootValuesAndConstructors is populated
                Ok { caModule with valueDefs = Dict.insert def.pattern def .valueDefs }

        FA.TypeAlias fa:
            At pos name =
                fa.name

            if Dict.member name caModule.aliasDefs or Dict.member name caModule.unionDefs then
                makeError pos [ name .. " declared twice!" ]

            else
                # TODO check args!
                translateType False ro fa.ty >> onOk type:

                aliasDef as CA.AliasDef = {
                    , usr = Meta.USR ro.currentModule (Pos.drop fa.name)
                    , args = fa.args
                    , type
                    , directTypeDeps = typeDeps type Set.empty
                    }

                Ok { caModule with aliasDefs = Dict.insert name aliasDef .aliasDefs }

        FA.UnionDef pos fa:
            if Dict.member fa.name caModule.aliasDefs or Dict.member fa.name caModule.unionDefs then
                makeError pos [ fa.name .. " declared twice!" ]

            else
                usr =
                    Meta.USR ro.currentModule fa.name

                type =
                    fa.args
                    >> List.map (name: CA.TypeAnnotationVariable pos name)
                    >> CA.TypeOpaque pos usr

                Dict.empty
                >> List.forRes fa.constructors (translateConstructor ro type usr)
                >> onOk constructors:

                unionDef as CA.UnionDef = {
                    , usr
                    , args = fa.args
                    , constructors
                    # I could probably break the deps by constructor, but would it be much useful in practice?
                    , directTypeDeps = Dict.for constructors (k: c: List.for c.args typeDeps) Set.empty
                    }

                Ok { caModule with unionDefs = Dict.insert fa.name unionDef .unionDefs }


translateModule as ReadOnly: Text: Meta.UniqueModuleReference: FA.Module: Res (CA.Module) =
    ro: asText: umr: faModule:

    Debug.benchStart None

    module =
        CA.initModule asText umr

    # Add all definitions
    module
    >> List.forRes faModule (insertRootStatement ro)
    >> btw Debug.benchStop "translateModule"

