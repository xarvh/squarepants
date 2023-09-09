Env =
    {
    , genVarCounter as Int
    , module as TA.Module
    }


#
# Translation
#

var PickedName =
    , 'trivialPattern Name TA.FullType
    , 'generateName
    , 'noNamedVariables


generateName as fn Env: Name & Env =
    fn env:
    Text.fromNumber (env.genVarCounter + 1) & { env with genVarCounter = 1 + .genVarCounter }


pickMainName as fn TA.Pattern: PickedName =
    fn pattern:
    try pattern as

        TA.'patternAny pos { maybeName = 'just name, type }:
            'trivialPattern name type

        _:
            if TA.patternNames pattern /= Dict.empty then
                'generateName
            else
                'noNamedVariables


translatePattern as fn TA.Pattern, EA.Expression: [ TA.FullType & Name & EA.Expression ] =
    fn pattern, accessExpr:
    translatePatternRec pattern accessExpr []


# TODO we can do this optimization only once we can replace the pattern variable name with the accessExpr variable in the block that uses it
#    try pattern as
#        TA.PatternAny _ _ _: []
#        _: translatePatternRec pattern accessExpr []

translatePatternRec as fn TA.Pattern, EA.Expression, [ TA.FullType & Name & EA.Expression ]: [ TA.FullType & Name & EA.Expression ] =
    fn pattern, accessExpr, accum:
    try pattern as

        TA.'patternAny _ { maybeName = 'nothing, type }:
            accum

        TA.'patternAny _ { maybeName = 'just name, type }:
            type & name & accessExpr :: accum

        TA.'patternLiteralNumber _ _:
            accum

        TA.'patternLiteralText _ _:
            accum

        TA.'patternConstructor _ path pas:
            accum
            >> List.indexedFor __ pas fn index, pa, a:
                translatePatternRec pa (EA.'constructorAccess index accessExpr) a

        TA.'patternRecord _ attrs:
            accum
            >> Dict.for __ attrs fn name, pa & type, a:
                translatePatternRec pa (EA.'recordAccess name accessExpr) a


testPattern as fn TA.Pattern, EA.Expression, [ EA.Expression ]: [ EA.Expression ] =
    fn pattern, valueToTest, accum:
    try pattern as

        TA.'patternAny _ _:
            accum

        TA.'patternLiteralText _ text:
            EA.'shallowEqual (EA.'literalText text) valueToTest :: accum

        TA.'patternLiteralNumber _ num:
            EA.'shallowEqual (EA.'literalNumber num) valueToTest :: accum

        TA.'patternConstructor _ ('USR umr name) pas:
            EA.'isConstructor name valueToTest :: accum
            >> List.indexedFor __ pas fn index, argPattern, a:
                testPattern argPattern (EA.'constructorAccess index valueToTest) a

        TA.'patternRecord _ attrs:
            accum
            >> Dict.for __ attrs fn name, pa & type, a:
                testPattern pa (EA.'recordAccess name valueToTest) a


translateParameter as fn Env, EA.Expression, TA.Parameter: EA.Expression & (Bool & Maybe Name) =
    fn env, bodyAcc, param:
    try param as

        TA.'parameterRecycle pos rawType name:
            bodyAcc & ('true & 'just name)

        TA.'parameterPlaceholder fullType n:
            bodyAcc & ('false & 'just (Text.fromNumber n))

        TA.'parameterPattern fullType pa:
            try pickMainName pa as

                'noNamedVariables:
                    bodyAcc & ('false & 'nothing)

                'trivialPattern argName type:
                    bodyAcc & ('false & 'just argName)

                'generateName:
                    mainName & newEnv =
                        generateName env

                    namesAndExpressions =
                        translatePattern pa (EA.'variable ('refLocal mainName))

                    wrapWithArgumentLetIn =
                        fn type & varName & letExpression, inExpression:
                        EA.'letIn
                            {
                            , inExpression
                            , letExpression
                            , maybeName = 'just varName
                            , type
                            }

                    List.for bodyAcc namesAndExpressions wrapWithArgumentLetIn & ('false & 'just mainName)


translateArgAndType as fn Env, TA.Argument: EA.Argument =
    fn env, taArg:
    try taArg as
        TA.'argumentExpression fullType exp: EA.'argumentSpend fullType (translateExpression env exp)
        TA.'argumentRecycle pos rawType attrPath name: EA.'argumentRecycle rawType attrPath name


translateExpression as fn Env, TA.Expression: EA.Expression =
    fn env, expression:
    try expression as

        TA.'literalNumber _ num:
            EA.'literalNumber num

        TA.'literalText _ text:
            EA.'literalText text

        TA.'variable _ ref:
            EA.'variable ref

        TA.'constructor _ usr:
            EA.'constructor usr

        TA.'recordAccess _ attrName exp:
            EA.'recordAccess attrName (translateExpression env exp)

        TA.'fn pos taPars body bodyT:
            eaBody =
                translateExpression { env with genVarCounter = List.length taPars + .genVarCounter } body

            wrappedBody & eaPars =
                eaBody & []
                >> List.forReversed __ taPars fn taPar, bodyAcc & eaParsAcc:
                    bodyX & eaPar =
                        newEnv =
                            { env with genVarCounter = List.length eaParsAcc + .genVarCounter }

                        translateParameter newEnv bodyAcc taPar

                    bodyX & (eaPar :: eaParsAcc)

            EA.'fn eaPars wrappedBody

        TA.'record _ extends attrs:
            attrs
            >> Dict.toList
            >> List.sortBy Tuple.first __
            >> List.map (Tuple.mapSecond (translateExpression env __) __) __
            >> EA.'literalRecord (Maybe.map (translateExpression env __) extends) __

        TA.'call _ ref argsAndTypes:
            EA.'call (translateExpression env ref) (List.map (translateArgAndType env __) argsAndTypes)

        TA.'if _ ar:
            EA.'conditional (translateExpression env ar.condition) (translateExpression env ar.true) (translateExpression env ar.false)

        TA.'try pos { patternsAndExpressions, value, valueType }:
            # 1. create a name for the value (unless it's already a variable)
            valueExpression & wrapWithLetIn & newEnv =
                try value & valueType.uni as

                    TA.'variable _ ref & 'imm:
                        EA.'variable ref & identity & env

                    _:
                        tryName & env_ =
                            generateName env

                        wrap =
                            fn tryExpression:
                            EA.'letIn
                                {
                                , inExpression = tryExpression
                                , letExpression = translateExpression env_ value
                                , maybeName = 'just tryName
                                , type = valueType
                                }

                        EA.'variable ('refLocal tryName) & wrap & env_

            # 2. if-elses
            addTryPatternAndBlock as fn TA.Pattern & TA.Expression, EA.Expression: EA.Expression =
                fn pattern & block, nextTryExpression:
                testIfPatternMatches as EA.Expression =
                    testPattern pattern valueExpression []
                    >> List.reverse
                    >> EA.'and

                namesAndExpressions as [ TA.FullType & Name & EA.Expression ] =
                    translatePattern pattern valueExpression

                whenConditionMatches as EA.Expression =
                    translateExpression newEnv block
                    >> List.for __ namesAndExpressions fn type & name & letExpression, inExpression:
                        EA.'letIn { inExpression, letExpression, maybeName = 'just name, type }

                EA.'conditional testIfPatternMatches whenConditionMatches nextTryExpression

            default =
                human =
                    Error.posToHuman { content = env.module.asText, fsPath = env.module.fsPath } pos

                EA.'missingPattern human.location valueExpression

            default
            >> List.forReversed __ patternsAndExpressions addTryPatternAndBlock
            >> wrapWithLetIn

        TA.'letIn valueDef e bodyType:
            try pickMainName valueDef.pattern as

                'noNamedVariables:
                    EA.'letIn
                        {
                        , inExpression = translateExpression env e
                        , letExpression = translateExpression env valueDef.body
                        , maybeName = 'nothing
                        , type = valueDef.type
                        }

                'trivialPattern defName type:
                    EA.'letIn
                        {
                        , inExpression = translateExpression env e
                        , letExpression = translateExpression env valueDef.body
                        , maybeName = 'just defName
                        , type
                        }

                'generateName:
                    mainName & newEnv =
                        # TODO check if valueDef.body is just a variable
                        generateName env

                    namesAndExpressions =
                        translatePattern valueDef.pattern (EA.'variable ('refLocal mainName))

                    wrapWithUnpackedPatternVar as fn TA.FullType & Name & EA.Expression, EA.Expression: EA.Expression =
                        fn type & name & letExpression, inExpression:
                        EA.'letIn
                            {
                            , inExpression
                            , letExpression
                            , maybeName = 'just name
                            , type
                            }

                    wrapWithActualLetIn as fn EA.Expression: EA.Expression =
                        fn inExpression:
                        EA.'letIn
                            {
                            , inExpression
                            , letExpression = translateExpression newEnv valueDef.body
                            , maybeName = 'just mainName
                            , type = valueDef.type
                            }

                    translateExpression newEnv e
                    >> List.forReversed __ namesAndExpressions wrapWithUnpackedPatternVar
                    >> wrapWithActualLetIn

        TA.'destroyIn name e:
            translateExpression env e


translateRootValueDef as fn Env, TA.ValueDef, ByUsr EA.GlobalDefinition: ByUsr EA.GlobalDefinition =
    fn env, def, accum:
    deps =
        def.directValueDeps

    try pickMainName def.pattern as

        'noNamedVariables:
            accum

        'trivialPattern name type:
            usr =
                'USR env.module.umr name

            Dict.insert
                usr
                {
                , deps
                , expr = translateExpression env def.body
                , usr
                }
                accum

        'generateName:
            mainName & newEnv =
                generateName env

            mainUsr =
                'USR env.module.umr mainName

            mainDef as EA.GlobalDefinition =
                {
                , deps
                , expr = translateExpression newEnv def.body
                , usr = mainUsr
                }

            accum
            >> Dict.insert mainUsr mainDef __
            >> List.for __ (translatePattern def.pattern (EA.'variable ('refGlobal mainUsr))) fn type & name & expr, z:
                subUsr =
                    'USR env.module.umr name

                Dict.insert subUsr { deps = Set.ofOne mainUsr, expr, usr = subUsr } z


#
# Main
#

circularIsError as fn ByUsr EA.GlobalDefinition, [ USR ]: Bool =
    fn globalDefsByName, usrs:
    zzz =
        fn usr:
        try Dict.get usr globalDefsByName as

            'nothing:
                # native or some special stuff?
                'false

            'just globalDef:
                try globalDef.expr as
                    EA.'fn _ _: 'false
                    _: 'true

    List.any zzz usrs


circularToError as fn [ USR ]: Error =
    fn usrs:
    Error.'raw
        [
        , "circular dependency: "
        , List.map toHuman usrs...
        ]


translateAll as fn UMR, [ TA.Module ]: Res { defs as [ EA.GlobalDefinition ], entryUsr as USR } =
    fn entryModule, modules:
    Debug.benchStart 'none

    globalDefsByName as ByUsr EA.GlobalDefinition =
        Dict.empty
        >> List.for __ modules fn module, d:
            Dict.for d module.valueDefs fn _, def, a:
                env =
                    { genVarCounter = 0, module }

                translateRootValueDef env def a

    circulars & reorderedNames =
        RefHierarchy.reorder (fn globalDef: globalDef.deps) globalDefsByName

    Debug.benchStop "makeEmittable"

    errors =
        List.filter (circularIsError globalDefsByName __) circulars

    if errors /= [] then
        errors
        >> List.map circularToError __
        >> Error.'nested
        >> 'err
    else
        {
        , defs = List.filterMap (fn name: Dict.get name globalDefsByName) reorderedNames
        , entryUsr = 'USR entryModule "main"
        }
        >> 'ok
