
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
    # This is used to tell the user tha definitions must be in order
    #
    , futureNonRootValues as Dict Text Pos
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


initEnv ro =
    as ReadOnly: Env
    {
    , maybeShorthandTarget = Nothing
    , nonRootValues = Dict.empty
    , futureNonRootValues = Dict.empty
    , ro = ro
    , defsPath = []
    , tyvarRenames = Dict.empty
    }


#
# Errors
#


makeError pos msg =
    as Pos: [Text]: Res a
    Error.res pos fn errorEnv: msg



#
# Names resolution
#

maybeForeignUsr getter ro maybeModule name =
    as (Meta: Dict Text Meta.UniqueSymbolReference): ReadOnly: Maybe Name: Name: Maybe Meta.UniqueSymbolReference

    try maybeModule as
        Just moduleName:
            try Dict.get moduleName ro.meta.moduleVisibleAsToUmr as
                Just umr:
                    Just << Meta.USR umr name

                Nothing:
                    # TODO should this produce an error instead?
                    # i.e., does ro.meta.moduleVisibleAsToUmr contain *all* modules, aliased or not?
                    #CA.RefRoot (Meta.USR Meta.SourcePlaceholder moduleName name)
                    Debug.todo "resolveToUsr can't find the module?"

        Nothing:
            Dict.get name (getter ro.meta)


resolveToUsr getter ro maybeModule name =
    as (Meta: Dict Text Meta.UniqueSymbolReference): ReadOnly: Maybe Name: Name: Meta.UniqueSymbolReference

    maybeForeignUsr getter ro maybeModule name
        >> Maybe.withDefault (Meta.USR ro.currentModule name)


resolveToValueRef ro declaredInsideFunction maybeModule name =
    as ReadOnly: Bool: Maybe Name: Name: CA.Ref

    try maybeForeignUsr (fn m: m.globalValues) ro maybeModule name as
        Just usr:
            CA.RefRoot usr

        Nothing:
            if declaredInsideFunction:
                CA.RefBlock name

            else
                CA.RefRoot << Meta.USR ro.currentModule name


resolveToTypeUsr =
    as ReadOnly: Maybe Name: Name: Meta.UniqueSymbolReference
    resolveToUsr (fn m: m.globalTypes)


resolveToConstructorUsr =
    as ReadOnly: Maybe Name: Name: Meta.UniqueSymbolReference
    resolveToUsr (fn m: m.globalValues)



##
#- Definition
#


insertParamNames param =
    as CA.Parameter: Dict Text Pos: Dict Text Pos
    try param as
        CA.ParameterMutable pos n:
            Dict.insert n pos

        CA.ParameterPattern pa:
            CA.patternNames pa >> Dict.join


translateDefinition isRoot env fa =
    as Bool: Env: FA.ValueDef: Res CA.ValueDef

    # This pattern is probably horrible, but I still want to experiment with it
    # How much can it actually be abused?
    # How much of a slippery slope is it?
    # How much will people use this rather than learn functional patterns?
    #
    # I could remove it by having translateType also return an env
    #
    # TODO this should transform the Dict into a mutation-friendly type, Dict is terrible with mutation
    dict @= env.tyvarRenames

    renameTyvar pos faName =
        as Pos: Text: Text

        try Dict.get faName dict as
            Just n: n
            Nothing:
                n = Text.fromNumber (Pos.start pos) .. faName
                @dict := Dict.insert faName n dict
                n

    translatePattern (Just renameTyvar) env fa.pattern >> onOk fn pattern:

    nonRootValues1 =
        if isRoot:
            env.nonRootValues

        else
            Dict.join (CA.patternNames pattern) env.nonRootValues

    localEnv0 =
        { env with
            , nonRootValues = nonRootValues1
            , defsPath = pattern :: .defsPath
            , tyvarRenames = dict
        }

    updNonFn tyvarName nonFn =
        try Dict.get tyvarName localEnv0.tyvarRenames as
            Nothing:
                # TODO should use the nonFn pos
                makeError (CA.patternPos pattern) [ "non fn on variable that's not in the annotation" ]

            Just tr:
                Ok << Dict.insert tr None nonFn

    List.foldlRes updNonFn fa.nonFn Dict.empty >> onOk fn nonFn:

    translateStatementBlock localEnv0 fa.body >> onOk fn body:
    Ok
        { pattern
        , native = False
        , mutable = fa.mutable
        , parentDefinitions = env.defsPath
        , nonFn
        , body
        }



#
# Pattern
#


translatePattern ann env fa =
    as Maybe (Pos: Text: Text): Env: FA.Pattern: Res CA.Pattern
    try fa as
        FA.PatternAny pos True s _:
            # TODO this happens (to me) when I use `=` in place of `:=`, so maybe change the message?
            makeError pos [ "This is the wrong place to use `@`" ]

        FA.PatternAny pos False name maybeFaType:
            if ann == Nothing and maybeFaType /= Nothing:
                makeError pos [ "Can't use annotations here" ]
            else
                Maybe.mapRes (translateType ann env.ro) maybeFaType >> onOk fn maybeCaType:

                n = if name == "_": Nothing else Just name
                Ok << CA.PatternAny pos n maybeCaType

        FA.PatternLiteralNumber pos l:
            translateNumber CA.PatternLiteralNumber pos l

        FA.PatternLiteralText pos l:
            Ok << CA.PatternLiteralText pos l

        FA.PatternConstructor pos maybeModule name faArgs:
            List.mapRes (translatePattern ann env) faArgs >> onOk fn caArgs:
            Ok << CA.PatternConstructor pos (resolveToConstructorUsr env.ro maybeModule name) caArgs

        FA.PatternList pos fas:
            fold pattern last =
                # TODO pos is probably inaccurate
                CA.PatternConstructor pos CoreTypes.cons [ pattern, last ]

            List.mapRes (translatePattern ann env) fas >> onOk fn cas:
            Ok << List.foldr fold cas (CA.PatternConstructor pos CoreTypes.nil [])

        FA.PatternRecord pos recordArgs:
            if recordArgs.extends /= Nothing:
                makeError pos [ "can't use `with` inside patterns" ]

            else
                fold ( (At p name) & maybePattern ) dict =
                    if Dict.member name dict:
                        makeError p [ "duplicate attribute name in pattern: " .. name ]

                    else
                        try maybePattern as
                            Nothing:
                                Ok << Dict.insert name (CA.PatternAny p (Just name) Nothing) dict

                            Just faPattern:
                                faPattern
                                    >> translatePattern ann env
                                    >> Result.map (fn caPattern: Dict.insert name caPattern dict)

                List.foldlRes fold recordArgs.attrs Dict.empty
                    >> Result.map (fn x: x >> CA.PatternRecord pos)

        FA.PatternListCons pos pas:
            List.mapRes (translatePattern ann env) pas >> onOk fn caPas:
            try List.reverse caPas as
                last :: rest:
                     last
                        >> List.foldl (fn item list: CA.PatternConstructor pos CoreTypes.cons [ item, list ]) rest
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


translateParameter ann env faParam =
    as Maybe (Pos: Text: Text): Env: FA.Pattern: Res CA.Parameter
    try faParam as
        FA.PatternAny pos True name Nothing:
            Ok << CA.ParameterMutable pos name

        FA.PatternAny pos True name _:
            makeError pos [ "Can't annotate this. =(", "TODO link to rationale for forbidding annotations" ]

        _:
            translatePattern ann env faParam >> onOk fn caPattern:
            Ok << CA.ParameterPattern caPattern



#
# Statement
#


translateStatementBlock env stats =
    as Env: [FA.Statement]: Res [CA.Statement]

    insertNames stat futureNonRootValues =
        try stat as
            FA.Definition pos def:
                Dict.join (FA.patternNames def.pattern) futureNonRootValues
            _:
                futureNonRootValues

    lEnv0 =
        as Env
        { env with futureNonRootValues = List.foldl insertNames stats env.futureNonRootValues }


    insertCaStatement faStat (lEnvX & caStats) =
        as FA.Statement: Env & [CA.Statement]: Res (Env & [CA.Statement])

        translateStatement lEnvX faStat
            >> Result.map (fn x: x >> Tuple.mapSecond ((::) caStats))

    lEnv0 & []
        >> List.foldlRes insertCaStatement stats
        >> Result.map (fn x: x >> Tuple.second >> List.reverse)


translateStatement env faStat =
    as Env: FA.Statement: Res (Env & CA.Statement)
    try faStat as
        FA.Evaluation pos faExpr:
            # TODO Non-return, non-mutable, non-debug evaluations should produce an error.
            # Debug evaluations should be optimized away in production build
            translateExpression env faExpr >> onOk fn e:
            Ok << env & CA.Evaluation e

        FA.Definition pos fa:
            translateDefinition False env fa >> onOk fn d:
            Ok << { env with nonRootValues = Dict.join (CA.patternNames d.pattern) .nonRootValues } & CA.Definition d

        FA.TypeAlias fa:
            At pos _ = fa.name
            makeError pos [ "Aliases can be declared only in the root scope" ]

        FA.UnionDef pos fa:
            makeError pos [ "Types can be declared only in the root scope" ]



##
#- Expression
#

translateExpression env faExpr =
    as Env: FA.Expression: Res CA.Expression
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

        FA.Lambda pos faParam faBody:
            translateParameter Nothing env faParam >> onOk fn caParam:
            localEnv =
                { env with nonRootValues = insertParamNames caParam .nonRootValues }
            translateStatementBlock localEnv faBody >> onOk fn caBody:
            Ok << CA.Lambda pos caParam caBody

        FA.FunctionCall pos reference arguments:
            # ref arg1 arg2 arg3...
            fold argument refAccum =
                as CA.Argument: CA.Expression: CA.Expression
                CA.Call pos refAccum argument

            translateExpression env reference >> onOk fn ref:
            List.mapRes (translateArgument env) arguments >> onOk fn args:
            Ok << List.foldl fold args ref

        FA.If pos { condition, true, false }:
            translateExpression env condition >> onOk fn c:
            translateStatementBlock env true >> onOk fn t:
            translateStatementBlock env false >> onOk fn f:
            { condition = [ CA.Evaluation c ]
            , true = t
            , false = f
            }
                >> CA.If pos
                >> Ok

        FA.Unop pos op faOperand:
            translateExpression env faOperand >> onOk fn caOperand:
            CA.Call pos
                (CA.Variable pos { ref = CA.RefRoot << CoreTypes.makeUsr op.symbol, attrPath = [] })
                (CA.ArgumentExpression caOperand)
            >> Ok

        FA.Binop pos group sepList:
            translateBinops env pos group sepList

        FA.Record pos faArgs:
            makeUpdateTarget pos env faArgs.extends >> onOk fn caUpdateTarget:
            # TODO use a Result.list_fold?
            translateAttrsRec { env with maybeShorthandTarget = caUpdateTarget.maybeName } faArgs.attrs Dict.empty >> onOk fn caAttrs:
            caAttrs
                >> CA.Record pos caUpdateTarget.maybeName
                >> caUpdateTarget.wrapper
                >> Ok

        FA.List pos faItems:
            cons item list =
                CA.Call pos
                    (CA.Call pos
                        (CoreTypes.usrToVariable CoreTypes.cons)
                        (CA.ArgumentExpression item)
                    )
                    (CA.ArgumentExpression list)

            List.mapRes (translateExpression env) faItems >> onOk fn es:
            Ok << List.foldr cons es (CoreTypes.usrToVariable CoreTypes.nil)

        FA.Try pos fa:
            translatePatternAndStatements ( faPattern & faStatements ) =
                translatePattern Nothing env faPattern >> onOk fn caPattern:
                translateStatementBlock { env with nonRootValues = Dict.join (CA.patternNames caPattern) env.nonRootValues } faStatements >> onOk fn block:
                Ok ( caPattern & block )

            translateExpression env fa.value >> onOk fn caValue:
            List.mapRes translatePatternAndStatements fa.patterns >> onOk fn caPatternsAndStatements:
            Ok << CA.Try pos caValue caPatternsAndStatements


translateNumber constructor pos numberAsText =
    as (Pos: Number: a): Pos: Text: Res a

    try Text.toNumber numberAsText as
        Nothing:
            makeError pos [
                , "invalid number: `" .. numberAsText .. "`"
                , "TODO link to documentation on valid number formats"
                ]

        Just n:
            Ok << constructor pos n


makeUpdateTarget pos env maybeShorthandTarget =
    as Pos: Env: Maybe FA.Expression: Res { maybeName as Maybe CA.VariableArgs, wrapper as CA.Expression: CA.Expression }
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


translateAttrsRec env faAttrs caAttrsAccum =
    as Env: [(At Text) & Maybe FA.Expression]: Dict Text CA.Expression: Res (Dict Text CA.Expression)
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

            exprRes >> onOk fn expr:
            translateAttrsRec env faTail (Dict.insert attrName expr caAttrsAccum)


translateArgument env faExpr =
    as Env: FA.Expression: Res CA.Argument
    try faExpr as
        FA.Mutable pos name attrPath:

            if Dict.member name env.nonRootValues:
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


translateBinops env pos group ( firstItem & firstTail ) =
    as Env: Pos: Op.Precedence: FA.SepList Op.Binop FA.Expression: Res CA.Expression
    try firstTail as
        []:
            translateExpression env firstItem

        (firstSep & secondItem) :: []:
            try group as
                Op.Tuple:
                    translateExpression env firstItem >> onOk fn first:
                    translateExpression env secondItem >> onOk fn second:
                    Dict.empty
                        >> Dict.insert "first" first
                        >> Dict.insert "second" second
                        >> CA.Record pos Nothing
                        >> Ok

                _:
                    translateSimpleBinop env pos firstItem firstSep secondItem

        (firstSep & secondItem) :: (secondSep & thirdItem) :: thirdTail:

            secondTail =
                as [Op.Binop & FA.Expression]
                (secondSep & thirdItem) :: thirdTail

            try group as
                Op.Comparison:
                    if notAllSeparators (sameDirectionAs firstSep) secondTail:
                        # TODO actually list the seps
                        makeError pos [ "can't mix comparison ops with different direction" ]

                    else
                        # TODO expand `a < b < c` to `a < b and b < c` without calculating b twice
                        makeError pos [ "NI compops expansion" ]

                Op.Logical:
                    if notAllSeparators ((==) firstSep) secondTail:
                        makeError pos [ "Mixing `and` and `or` is ambiguous. Use parens!" ]

                    else
                        translateBinopSepList_rightAssociative env pos firstItem firstTail

                Op.Tuple:
                    if thirdTail /= []:
                        makeError pos [ "Tuples can't have more than 3 items, use a record instead." ]

                    else
                         translateExpression env firstItem >> onOk fn first:
                         translateExpression env secondItem >> onOk fn second:
                         translateExpression env thirdItem >> onOk fn third:
                         Dict.empty
                             >> Dict.insert "first" first
                             >> Dict.insert "second" second
                             >> Dict.insert "third" third
                             >> CA.Record pos Nothing
                             >> Ok

                Op.Pipe:
                    if notAllSeparators ((==) firstSep) secondTail:
                        makeError pos [ "Mixing pipes is ambigous. Use parens." ]

                    else if firstSep.associativity == Op.Right:
                        translateBinopSepList_rightAssociative env pos firstItem firstTail

                    else
                        translateBinopSepList_leftAssociative env pos firstItem firstTail

                Op.Mutop:
                    makeError pos [ "mutops can't be chained" ]

                _:
                    translateBinopSepList_rightAssociative env pos firstItem firstTail


notAllSeparators f ls =
    as (sep: Bool): [ sep & item]: Bool
    try ls as
        []:
            False

        (sep & item) :: tail:
            if f sep:
                notAllSeparators f tail

            else
                True


sameDirectionAs a b =
    as Op.Binop: Op.Binop: Bool
    if a.symbol == b.symbol:
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


translateBinopSepList_rightAssociative env pos left opsAndRight =
    as Env: Pos: FA.Expression: [ Op.Binop & FA.Expression ]: Res CA.Expression
    translateExpression env left >> onOk fn caLeft:
    try opsAndRight as
        []:
            Ok caLeft

        (op & right) :: tail:
            translateBinopSepList_rightAssociative env pos right tail >> onOk fn caRight:
            Ok << makeBinop pos (CA.ArgumentExpression caLeft) op (CA.ArgumentExpression caRight)


translateBinopSepList_leftAssociative env pos leftAccum opsAndRight =
    as Env: Pos: FA.Expression: [ Op.Binop & FA.Expression ]: Res CA.Expression

    translateExpression env leftAccum >> onOk fn caLeftAccum:
    translateBinopSepListRec env pos caLeftAccum opsAndRight


translateBinopSepListRec env pos leftAccum opsAndRight =
    as Env: Pos: CA.Expression: [ Op.Binop & FA.Expression ]: Res CA.Expression
    try opsAndRight as
        []:
            Ok leftAccum

        (op & faRight) :: tail:
            translateArgument env faRight >> onOk fn caRight:
            translateBinopSepListRec env pos (makeBinop pos (CA.ArgumentExpression leftAccum) op caRight) tail


[# Unlike other ML languages, the left operand is the _second_ argument

`a + b` == `((+) b) a`

#]
makeBinop pos left op right =
    as Pos: CA.Argument: Op.Binop: CA.Argument: CA.Expression
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
                        , ref = CA.RefRoot (Meta.spCoreUSR op.symbol)
                        , attrPath = []
                        }
                    )
                    right
                )
                left


translateSimpleBinop env pos left op right =
    as Env: Pos: FA.Expression: Op.Binop: FA.Expression: Res CA.Expression

    translateArgument env left >> onOk fn l:
    translateArgument env right >> onOk fn r:
    Ok << makeBinop pos l op r



#
# Type
#


addAttributes ro pos faAttrs caAttrsAccum =
    as ReadOnly: Pos: [ (At Text) & Maybe FA.Type ]: Dict Text CA.Type: Res CA.Type
    try faAttrs as
        []:
            CA.TypeRecord pos Nothing caAttrsAccum
                >> Ok

        ((At p name) & maybeFaType) :: faTail:
            try maybeFaType as
                Nothing:
                    makeError p [ "Attribute `" .. name .. "` must have a type" ]

                Just faType:
                    translateType Nothing ro faType >> onOk fn caType:
                    addAttributes ro p faTail (Dict.insert name caType caAttrsAccum)


#
# `mrf`: maybe rename function, used by translatePatterns to ensure annotated tyvar names are unique within the module
#
translateType mrf ro faType =
    as Maybe (Pos: Text: Text): ReadOnly: FA.Type: Res CA.Type

    try faType as
        FA.TypeVariable pos name:
            try mrf as
                Nothing:
                    Ok << CA.TypeVariable pos name

                Just renameFunction:
                    Ok << CA.TypeVariable pos (renameFunction pos name)

        FA.TypeConstant pos maybeModule name args:
            List.mapRes (translateType mrf ro) args >> onOk fn caArgs:
            caArgs
                >> CA.TypeConstant pos (resolveToTypeUsr ro maybeModule name)
                >> Ok

        FA.TypeFunction pos fa_from fromIsMut fa_to:
            translateType mrf ro fa_from >> onOk fn ca_from:
            translateType mrf ro fa_to >> onOk fn ca_to:
            Ok << CA.TypeFunction pos ca_from fromIsMut ca_to

        FA.TypeTuple pos types:
            try types as
                [ faFirst, faSecond ]:
                    translateType mrf ro faFirst >> onOk fn caFirst:
                    translateType mrf ro faSecond >> onOk fn caSecond:
                    Dict.empty
                        >> Dict.insert "first" caFirst
                        >> Dict.insert "second" caSecond
                        >> CA.TypeRecord pos Nothing
                        >> Ok

                [ faFirst, faSecond, faThird ]:
                    translateType mrf ro faFirst >> onOk fn caFirst:
                    translateType mrf ro faSecond >> onOk fn caSecond:
                    translateType mrf ro faThird >> onOk fn caThird:
                    Dict.empty
                        >> Dict.insert "first" caFirst
                        >> Dict.insert "second" caSecond
                        >> Dict.insert "third" caThird
                        >> CA.TypeRecord pos Nothing
                        >> Ok

                _:
                    makeError pos [ "Tuples can only have size 2 or 3. Use a record." ]

        FA.TypeList pos faItem:
            translateType mrf ro faItem >> onOk fn caItem:
            Ok << CoreTypes.list caItem

        FA.TypeRecord p recordArgs:
            if recordArgs.extends /= Nothing:
                makeError p ["For now extensible types are disabled, I want to see if it's good to do without them" ]

            else
                addAttributes ro p recordArgs.attrs Dict.empty


#
# Union constructor
#


translateConstructor ro unionType (At pos name & faArgs) constructors =
    as ReadOnly: CA.Type: At Name & [FA.Type]: Dict Name CA.Constructor: Res (Dict Name CA.Constructor)

    if Dict.member name constructors:
        # TODO "union $whatever has two constructors with the same name!"
        makeError pos [ "constructor " .. name .. " is duplicate" ]

    else
        List.mapRes (translateType Nothing ro) faArgs >> onOk fn caArgs:

        c =
            as CA.Constructor
            {
            , pos
            , type = List.foldr (fn ar ty: CA.TypeFunction pos ar False ty) caArgs unionType
            , args = caArgs
            }

        Ok << Dict.insert name c constructors


#
# Module
#


insertRootStatement ro faStatement caModule =
    as ReadOnly: FA.Statement: CA.Module: Res CA.Module
    try faStatement as
        FA.Evaluation pos expr:
            makeError (FA.expressionPos expr) [ "Root Evaluations don't really do much =|" ]

        FA.Definition pos fa:
            translateDefinition True (initEnv ro) fa >> onOk fn def:
            if def.mutable:
                makeError (CA.patternPos def.pattern) [ "Mutable values can be declared only inside functions." ]

            else
                # Patterns contain position, so they are unique and don't need to be checked for duplication
                # Names duplication will be checked when rootValuesAndConstructors is populated
                Ok { caModule with valueDefs = Dict.insert def.pattern def .valueDefs }

        FA.TypeAlias fa:
            At pos name =
                fa.name

            if Dict.member name caModule.aliasDefs or Dict.member name caModule.unionDefs:
                makeError pos [ name .. " declared twice!" ]

            else
                # TODO check args!
                translateType Nothing ro fa.ty >> onOk fn type:

                aliasDef =
                    as CA.AliasDef
                    {
                    , usr = Meta.USR ro.currentModule (Pos.drop fa.name)
                    , args = fa.args
                    , type
                    }

                Ok { caModule with aliasDefs = Dict.insert name aliasDef .aliasDefs }

        FA.UnionDef pos fa:
            if Dict.member fa.name caModule.aliasDefs or Dict.member fa.name caModule.unionDefs:
                makeError pos [ fa.name .. " declared twice!" ]

            else
                usr =
                    Meta.USR ro.currentModule fa.name

                type =
                    fa.args
                        >> List.map (CA.TypeVariable pos)
                        >> CA.TypeConstant pos usr

                List.foldlRes (translateConstructor ro type) fa.constructors Dict.empty >> onOk fn constructors:

                unionDef =
                    as CA.UnionDef
                    {
                    , usr
                    , args = fa.args
                    , constructors
                    }

                Ok { caModule with unionDefs = Dict.insert fa.name unionDef .unionDefs }


translateModule ro asText umr faModule =
    as ReadOnly: Text: Meta.UniqueModuleReference: FA.Module: Res CA.Module

    module =
        CA.initModule asText umr

    # Add all definitions
    List.foldlRes (insertRootStatement ro) faModule module
