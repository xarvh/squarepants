
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
    , maybeShorthandTarget as Maybe CA.VariableArgs
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
    , defsPath as [CA.Pattern]
    #
    , ro as ReadOnly
    [#
        Unlike instance variables, type variables will be used (for typecheking) across scopes.
        To avoid mixing up different tyvars with the same name, we need to rename them in a way that makes them unique, at least within the module.

        We need to remember this renaming from the definition where the tyvar is first used to all the children scopes of the definition.
    #]
    , tyvarRenames as Dict Text Text
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
    , defsPath = []
    , tyvarRenames = Dict.empty
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
                    #CA.RefRoot (Meta.USR Meta.SourcePlaceholder moduleName name)
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
            CA.RefRoot usr

        Nothing:
            if declaredInsideFunction then
                CA.RefBlock name

            else
                CA.RefRoot << Meta.USR ro.currentModule name


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
        CA.TypeConstant _ usr args: acc >> Set.insert usr >> List.for args typeDeps
        CA.TypeVariable _ _: acc
        CA.TypeFunction _ from _ to: acc >> typeDeps from >> typeDeps to
        CA.TypeRecord _ _ attrs: Dict.for attrs (k: typeDeps) acc
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


patternDeps as CA.Pattern: Deps: Deps =
    pattern: deps:

    try pattern as

        CA.PatternConstructor _ usr ps:
            List.for ps patternDeps { deps with cons = Set.insert usr .cons }

        CA.PatternRecord _ ps:
            Dict.for ps (k: patternDeps) deps

        CA.PatternAny _ mutability maybeName (Just type):
            { deps with types = typeDeps type .types }

        CA.PatternAny _ mutability maybeName Nothing:
            deps

        CA.PatternLiteralNumber _ _:
           deps

        CA.PatternLiteralText _ _:
           deps


expressionDeps as CA.Expression: Deps: Deps =
    expr: deps:
    try expr as
        CA.LiteralNumber _ _:
            deps

        CA.LiteralText _ _:
            deps

        CA.Variable _ { ref = CA.RefRoot usr, attrPath }:
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

        CA.Record _ (Just { ref = CA.RefRoot usr, attrPath }) exprByName:
            { deps with values = Set.insert usr .values }
                >> Dict.for exprByName (name: expressionDeps)

        CA.Record _ _ exprByName:
            deps
                >> Dict.for exprByName (name: expressionDeps)

        CA.Call _ e0 (CA.ArgumentExpression e1):
            deps
                >> expressionDeps e0
                >> expressionDeps e1

        CA.Call _ e0 (CA.ArgumentMutable _ _):
            deps
                >> expressionDeps e0

        CA.If _ args:
            deps
                >> expressionDeps args.condition
                >> expressionDeps args.true
                >> expressionDeps args.false

        CA.Try _ e patternsAndBodies:
            deps
                >> expressionDeps e
                >> List.for patternsAndBodies ((p & b): d: d >> patternDeps p >> expressionDeps b)

        CA.LetIn valueDef e:
            deps
                >> patternDeps valueDef.pattern
                >> expressionDeps valueDef.body
                >> expressionDeps e


#
# Definition
#


translateDefinition as Bool: Env: FA.ValueDef: Res CA.ValueDef =
    isRoot: env: fa:

    # This pattern is probably horrible, but I still want to experiment with it
    # How much can it actually be abused?
    # How much of a slippery slope is it?
    # How much will people use this rather than learn functional patterns?
    #
    # I could remove it by having translateType also return an env
    #
    # TODO this should transform the Dict into a mutation-friendly type, Dict is terrible with mutation
    dict @= env.tyvarRenames

    renameTyvar as Pos: Text: Text =
        pos: faName:
        try Dict.get faName dict as
            Just n: n
            Nothing:
                n = Text.fromNumber (Pos.start pos) .. faName
                @dict := Dict.insert faName n dict
                n

    translatePattern (Just renameTyvar) env fa.pattern >> onOk pattern:

    nonRootValues1 =
        if isRoot then
            env.nonRootValues

        else
            Dict.join (CA.patternNames pattern) env.nonRootValues

    localEnv0 =
        { env with
        , nonRootValues = nonRootValues1
        , defsPath = pattern :: .defsPath
        , tyvarRenames = dict
        }

    updNonFn = tyvarName: nonFn:
        try Dict.get tyvarName localEnv0.tyvarRenames as
            Nothing:
                # TODO should use the nonFn pos
                makeError (CA.patternPos pattern) [ "non fn on variable that's not in the annotation" ]

            Just tr:
                Ok << Dict.insert tr None nonFn

    Dict.empty
    >> List.forRes fa.nonFn updNonFn
    >> onOk nonFn:

    translateStatementBlock localEnv0 fa.body
    >> onOk body:

    deps =
        if isRoot then
            deps_init >> patternDeps pattern >> expressionDeps body
        else
            deps_init

    Ok {
        , pattern
        , native = False
        , parentDefinitions = env.defsPath
        , nonFn
        , body
        #
        , directTypeDeps = deps.types
        , directConsDeps = deps.cons
        , directValueDeps = deps.values
        }



#
# Pattern
#


translatePattern as Maybe (Pos: Text: Text): Env: FA.Pattern: Res CA.Pattern =
    ann: env: fa:
    try fa as
        FA.PatternAny pos isMutable name maybeFaType:
            if ann == Nothing and maybeFaType /= Nothing then
                makeError pos [ "Can't use annotations here" ]
            else
                Maybe.mapRes (translateType ann env.ro) maybeFaType >> onOk maybeCaType:

                maybeName =
                    if name == "_" then Nothing else (Just name)

                Ok << CA.PatternAny pos isMutable maybeName maybeCaType

        FA.PatternLiteralNumber pos l:
            translateNumber CA.PatternLiteralNumber pos l

        FA.PatternLiteralText pos l:
            Ok << CA.PatternLiteralText pos l

        FA.PatternConstructor pos maybeModule name faArgs:
            List.mapRes (translatePattern ann env) faArgs >> onOk caArgs:
            Ok << CA.PatternConstructor pos (resolveToConstructorUsr env.ro maybeModule name) caArgs

        FA.PatternList pos fas:
            fold = pattern: last:
                # TODO pos is probably inaccurate
                CA.PatternConstructor pos CoreTypes.cons [ pattern, last ]

            List.mapRes (translatePattern ann env) fas >> onOk cas:
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
                                Ok << Dict.insert name (CA.PatternAny p False (Just name) Nothing) dict

                            Just faPattern:
                                faPattern
                                    >> translatePattern ann env
                                    >> Result.map (caPattern: Dict.insert name caPattern dict)

                Dict.empty
                >> List.forRes recordArgs.attrs fold
                >> Result.map (x: x >> CA.PatternRecord pos)

        FA.PatternListCons pos pas:
            List.mapRes (translatePattern ann env) pas >> onOk caPas:
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
                        >> translatePattern ann env

                [ fa1, fa2, fa3 ]:
                    { extends = Nothing
                    , attrs =
                        [ (At (FA.patternPos fa1) "first") & Just fa1
                        , (At (FA.patternPos fa2) "second") & Just fa2
                        , (At (FA.patternPos fa3) "third") & Just fa3
                        ]
                    }
                        >> FA.PatternRecord pos
                        >> translatePattern ann env

                _:
                    makeError pos [ "tuples can be only of size 2 or 3" ]


#
# Statement
#
translateStatementBlock as Env: [FA.Statement]: Res CA.Expression =
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
translateExpression as Env: FA.Expression: Res CA.Expression =
    env: faExpr:
    try faExpr as
        FA.LiteralNumber pos str:
            translateNumber CA.LiteralNumber pos str

        FA.LiteralText pos v:
            Ok << CA.LiteralText pos v

        FA.PrefixBinop pos symbol:
            { ref = CA.RefRoot << CoreTypes.makeUsr symbol
            , attrPath = []
            }
                >> CA.Variable pos
                >> Ok

        FA.Variable pos maybeModule name attrs:
            declaredInsideFunction =
                Dict.member name env.nonRootValues

            { ref = resolveToValueRef env.ro declaredInsideFunction maybeModule name
            , attrPath = attrs
            }
                >> CA.Variable pos
                >> Ok

        FA.Constructor pos maybeModule name:
            resolveToConstructorUsr env.ro maybeModule name
                >> CA.Constructor pos
                >> Ok

        FA.Mutable pos name _:
            makeError pos [ name .. ": mutable values can be used only as arguments for function or mutation operators" ]

        FA.RecordShorthand pos attrPath:
            try env.maybeShorthandTarget as
                Nothing:
                    makeError pos [
                        , "Record update shorthands must be used inside a record update such as"
                        , "    { aRecord with anAttribute = doSomethingWith ." .. Text.join "." attrPath .. " }"
                        , "but we are not inside a record update!"
                        ]

                Just shorthandTarget:
                    # Yes, you can write
                    #
                    #   { blah.x.y with z0 = .z1.w.x }
                    #
                    # Is this a good idea?
                    Ok << CA.Variable pos { shorthandTarget with attrPath = List.concat [ .attrPath, attrPath ] }

        FA.Lambda pos faParam isConsuming faBody:

            translatePattern Nothing env faParam
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
                (CA.Variable pos { ref = CA.RefRoot << op.usr, attrPath = [] })
                (CA.ArgumentExpression caOperand)
            >> Ok

        FA.Binop pos group sepList:
            translateBinops env pos group sepList

        FA.Record pos faArgs:
            makeUpdateTarget pos env faArgs.extends >> onOk caUpdateTarget:
            # TODO use a Result.list_fold?
            translateAttrsRec { env with maybeShorthandTarget = caUpdateTarget.maybeName } faArgs.attrs Dict.empty >> onOk caAttrs:
            caAttrs
                >> CA.Record pos caUpdateTarget.maybeName
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
                translatePattern Nothing env faPattern >> onOk caPattern:
                translateStatementBlock { env with nonRootValues = Dict.join (CA.patternNames caPattern) env.nonRootValues } faStatements >> onOk block:
                Ok ( caPattern & block )

            translateExpression env fa.value >> onOk caValue:
            List.mapRes translatePatternAndStatements fa.patterns >> onOk caPatternsAndStatements:
            Ok << CA.Try pos caValue caPatternsAndStatements


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


makeUpdateTarget as Pos: Env: Maybe FA.Expression: Res { maybeName as Maybe CA.VariableArgs, wrapper as CA.Expression: CA.Expression } =
    pos: env: maybeShorthandTarget:
    try Maybe.map (translateExpression { env with maybeShorthandTarget = Nothing }) maybeShorthandTarget as
        Nothing:
            Ok { maybeName = Nothing, wrapper = identity }

        Just (Err e):
            Err e

        Just (Ok (CA.Variable _ args)):
            # TODO test for lowercase name?
            Ok { maybeName = Just args, wrapper = identity }

        Just (Ok expr):
            # TODO: can I use the start position as unique name?
            makeError pos [ "NI { (expr) with ...} not yet implemented =(" ]


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

                        { ref = resolveToValueRef env.ro declaredInsideFunction Nothing attrName
                        , attrPath = []
                        }
                            >> CA.Variable pos
                            >> Ok

            exprRes >> onOk expr:
            translateAttrsRec env faTail (Dict.insert attrName expr caAttrsAccum)


translateArgument as Env: FA.Expression: Res CA.Argument =
    env: faExpr:
    try faExpr as
        FA.Mutable pos name attrPath:

            if Dict.member name env.nonRootValues then
                {
                , ref = CA.RefBlock name
                , attrPath
                }
                    >> CA.ArgumentMutable pos
                    >> Ok

            else
                makeError pos
                    [ "only values declared inside a function scope can be mutated!"
                    ]

        _:
            faExpr
                >> translateExpression env
                >> Result.map CA.ArgumentExpression


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
                    (CA.Variable pos {
                        , ref = CA.RefRoot op.usr
                        , attrPath = []
                        }
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


addAttributes as ReadOnly: Pos: [ (At Text) & Maybe FA.Type ]: Dict Text CA.Type: Res CA.Type =
    ro: pos: faAttrs: caAttrsAccum:
    try faAttrs as
        []:
            CA.TypeRecord pos Nothing caAttrsAccum
                >> Ok

        ((At p name) & maybeFaType) :: faTail:
            try maybeFaType as
                Nothing:
                    makeError p [ "Attribute `" .. name .. "` must have a type" ]

                Just faType:
                    translateType Nothing ro faType >> onOk caType:
                    addAttributes ro p faTail (Dict.insert name caType caAttrsAccum)


#
# `mrf`: maybe rename function, used by translatePatterns to ensure annotated tyvar names are unique within the module
#
translateType as Maybe (Pos: Text: Text): ReadOnly: FA.Type: Res CA.Type =
    mrf: ro: faType:

    try faType as
        FA.TypeVariable pos name:
            try mrf as
                Nothing:
                    Ok << CA.TypeVariable pos name

                Just renameFunction:
                    Ok << CA.TypeVariable pos (renameFunction pos name)

        FA.TypeConstant pos maybeModule name args:
            List.mapRes (translateType mrf ro) args >> onOk caArgs:
            caArgs
                >> CA.TypeConstant pos (resolveToTypeUsr ro maybeModule name)
                >> Ok

        FA.TypeFunction pos fa_from fromIsMut fa_to:
            translateType mrf ro fa_from >> onOk ca_from:
            translateType mrf ro fa_to >> onOk ca_to:
            Ok << CA.TypeFunction pos ca_from fromIsMut ca_to

        FA.TypeTuple pos types:
            try types as
                [ faFirst, faSecond ]:
                    translateType mrf ro faFirst >> onOk caFirst:
                    translateType mrf ro faSecond >> onOk caSecond:
                    Dict.empty
                        >> Dict.insert "first" caFirst
                        >> Dict.insert "second" caSecond
                        >> CA.TypeRecord pos Nothing
                        >> Ok

                [ faFirst, faSecond, faThird ]:
                    translateType mrf ro faFirst >> onOk caFirst:
                    translateType mrf ro faSecond >> onOk caSecond:
                    translateType mrf ro faThird >> onOk caThird:
                    Dict.empty
                        >> Dict.insert "first" caFirst
                        >> Dict.insert "second" caSecond
                        >> Dict.insert "third" caThird
                        >> CA.TypeRecord pos Nothing
                        >> Ok

                _:
                    makeError pos [ "Tuples can only have size 2 or 3. Use a record." ]

        FA.TypeList pos faItem:
            translateType mrf ro faItem >> onOk caItem:
            Ok << CoreTypes.list caItem

        FA.TypeRecord p recordArgs:
            if recordArgs.extends /= Nothing then
                makeError p ["For now extensible types are disabled, I want to see if it's good to do without them" ]

            else
                addAttributes ro p recordArgs.attrs Dict.empty


#
# Union constructor
#


translateConstructor as ReadOnly: CA.Type: Meta.UniqueSymbolReference: At Name & [FA.Type]: Dict Name CA.Constructor: Res (Dict Name CA.Constructor) =
    ro: unionType: unionUsr: (At pos name & faArgs): constructors:

    if Dict.member name constructors then
        # TODO "union $whatever has two constructors with the same name!"
        makeError pos [ "constructor " .. name .. " is duplicate" ]

    else
        List.mapRes (translateType Nothing ro) faArgs >> onOk caArgs:

        c as CA.Constructor = {
            , pos
            , typeUsr = unionUsr
            , type = List.forReversed caArgs (ar: ty: CA.TypeFunction pos ar False ty) unionType
            , args = caArgs
            }

        Ok << Dict.insert name c constructors


#
# Module
#


insertRootStatement as ReadOnly: FA.Statement: CA.Module: Res CA.Module =
    ro: faStatement: caModule:
    try faStatement as
        FA.Evaluation pos expr:
            makeError (FA.expressionPos expr) [ "Root Evaluations don't really do much =|" ]

        FA.Definition pos fa:
            translateDefinition True (initEnv ro) fa
            >> onOk def:


            if CA.patternIsMutable def.pattern then
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
                translateType Nothing ro fa.ty >> onOk type:

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
                        >> List.map (CA.TypeVariable pos)
                        >> CA.TypeConstant pos usr

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


translateModule as ReadOnly: Text: Meta.UniqueModuleReference: FA.Module: Res CA.Module =
    ro: asText: umr: faModule:

    Debug.benchStart None

    module =
        CA.initModule asText umr

    # Add all definitions
    module
    >> List.forRes faModule (insertRootStatement ro)
    >> btw Debug.benchStop "translateModule"

