Env =
    {
    , genVarCounter as Int
    , module as CA.Module
    }


State =
    {
    , lambdaContextes as Hash EA.TranslatedUsr (Dict Name TA.FullType)
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
            EA.'isLiteralText text valueToTest :: accum

        TA.'patternLiteralNumber _ num:
            EA.'isLiteralNumber num valueToTest :: accum

        TA.'patternConstructor _ usr pas:
            EA.'isConstructor usr valueToTest :: accum
            >> List.indexedFor __ pas fn index, argPattern, a:
                testPattern argPattern (EA.'constructorAccess index valueToTest) a

        TA.'patternRecord _ attrs:
            accum
            >> Dict.for __ attrs fn name, pa & type, a:
                testPattern pa (EA.'recordAccess name valueToTest) a


translateParameter as fn Env, EA.Expression, TA.Parameter: EA.Expression & (TA.FullType & Maybe Name) =
    fn env, bodyAcc, param:
    try param as

        TA.'parameterRecycle pos raw name:
            bodyAcc & ({ raw, uni = 'uni } & 'just name)

        TA.'parameterPlaceholder fullType n:
            bodyAcc & (fullType & 'just (Text.fromNumber n))

        TA.'parameterPattern fullType pa:
            try pickMainName pa as

                'noNamedVariables:
                    bodyAcc & (fullType & 'nothing)

                'trivialPattern argName type:
                    bodyAcc & (fullType & 'just argName)

                'generateName:
                    mainName & newEnv =
                        generateName env

                    namesAndExpressions =
                        translatePattern pa (EA.'localVariable mainName)

                    wrapWithArgumentLetIn =
                        fn type & varName & letExpression, inExpression:
                        EA.'letIn
                            {
                            , inExpression
                            , letExpression
                            , maybeName = 'just varName
                            , type
                            }

                    List.for bodyAcc namesAndExpressions wrapWithArgumentLetIn & (fullType & 'just mainName)


translateArgAndType as fn Env, @State, TA.Argument: EA.Argument =
    fn env, @state, taArg:
    try taArg as
        TA.'argumentExpression fullType exp: EA.'argumentSpend fullType (translateExpression env @state exp)
        TA.'argumentRecycle pos rawType attrPath name: EA.'argumentRecycle rawType attrPath name


translateExpression as fn Env, @State, TA.Expression: EA.Expression =
    fn env, @state, expression:
    try expression as

        TA.'literalNumber _ num:
            EA.'literalNumber num

        TA.'literalText _ text:
            EA.'literalText text

        TA.'variable _ ('refLocal name):
            EA.'localVariable name

        TA.'variable _ ('refGlobal usr):
            EA.'globalVariable (EA.translateUsr usr TA.rootLambdaRef)

        TA.'variable _ ('refPlaceholder n):
            EA.'placeholderVariable n

        TA.'lambda _ (usr & id) context:
            tUsr =
                EA.translateUsr usr id

            Hash.insert @state.lambdaContextes tUsr context

            EA.'lambda tUsr context

        TA.'constructor _ usr:
            EA.'constructor (EA.translateUsr usr 0)

        TA.'recordAccess _ attrName exp:
            EA.'recordAccess attrName (translateExpression env @state exp)

#        TA.'fn pos _ _ taPars body bodyT:
#            eaBody =
#                translateExpression { env with genVarCounter = List.length taPars + .genVarCounter } body
#
#            wrappedBody & eaPars =
#                eaBody & []
#                >> List.forReversed __ taPars fn taPar, bodyAcc & eaParsAcc:
#                    bodyX & eaPar =
#                        newEnv =
#                            { env with genVarCounter = List.length eaParsAcc + .genVarCounter }
#
#                        translateParameter newEnv bodyAcc taPar
#
#                    bodyX & (eaPar :: eaParsAcc)
#
#            EA.'fn eaPars wrappedBody

        TA.'record _ extends attrs:
            attrs
            >> Dict.toList
            >> List.sortBy Tuple.first __
            >> List.map (Tuple.mapSecond (translateExpression env @state __) __) __
            >> EA.'literalRecord (Maybe.map (translateExpression env @state __) extends) __

        TA.'call _ _ ref argsAndTypes:
            EA.'call (translateExpression env @state ref) (List.map (translateArgAndType env @state __) argsAndTypes)

        TA.'if _ ar:
            EA.'conditional (translateExpression env @state ar.condition) (translateExpression env @state ar.true) (translateExpression env @state ar.false)

        TA.'try pos { patternsAndExpressions, value, valueType }:
            # 1. create a name for the value (unless it's already a variable)
            valueExpression & wrapWithLetIn & newEnv =
                try value & valueType.uni as

                    TA.'variable _ ('refLocal name) & 'imm:
                        EA.'localVariable name & identity & env

                    TA.'variable _ ('refGlobal usr) & 'imm:
                        EA.'globalVariable (EA.translateUsr usr 0) & identity & env

                    TA.'variable _ ('refPlaceholder n) & 'imm:
                        EA.'placeholderVariable n & identity & env

                    _:
                        tryName & env_ =
                            generateName env

                        wrap =
                            fn tryExpression:
                            EA.'letIn
                                {
                                , inExpression = tryExpression
                                , letExpression = translateExpression env_ @state value
                                , maybeName = 'just tryName
                                , type = valueType
                                }

                        EA.'localVariable tryName & wrap & env_

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
                    translateExpression newEnv @state block
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
                        , inExpression = translateExpression env @state e
                        , letExpression = translateExpression env @state valueDef.body
                        , maybeName = 'nothing
                        , type = valueDef.type
                        }

                'trivialPattern defName type:
                    EA.'letIn
                        {
                        , inExpression = translateExpression env @state e
                        , letExpression = translateExpression env @state valueDef.body
                        , maybeName = 'just defName
                        , type
                        }

                'generateName:
                    mainName & newEnv =
                        # TODO check if body is just a variable
                        generateName env

                    namesAndExpressions =
                        translatePattern valueDef.pattern (EA.'localVariable mainName)

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
                            , letExpression = translateExpression newEnv @state valueDef.body
                            , maybeName = 'just mainName
                            , type = valueDef.type
                            }

                    translateExpression newEnv @state e
                    >> List.forReversed __ namesAndExpressions wrapWithUnpackedPatternVar
                    >> wrapWithActualLetIn

        TA.'destroyIn name e:
            translateExpression env @state e

        TA.'introspect self:
            EA.'introspect self


translateRootDef as fn Dict UMR CA.Module, USR, TA.RootDef: [ EA.GlobalDefinition ] =
    fn modulesByUmr, usr, def:
    try def.body as

        'nothing:
            []

        'just body:
            'USR umr name =
                usr

            env as Env =
                {
                , genVarCounter = 0
                , module =
                    try Dict.get umr modulesByUmr as
                        'just m: m
                        'nothing: todo ("compiler bug: no module for " .. name)
                }

            !state as State =
                {
                , lambdaContextes = Hash.fromList []
                }

            valueDef as EA.GlobalDefinition =
                {
                , context = Dict.empty
                , deps = def.directDeps
                , expr = translateExpression env @state body
                , freeTyvars = def.freeTyvars
                , freeUnivars = def.freeUnivars
                , parameters = []
                , returnType = { raw = def.type, uni = 'imm }
                , usr = EA.translateUsr usr TA.rootLambdaRef
                }

            Dict.for [ valueDef ] def.lambdas fn index, lambda, defs:
                baseBody =
                    translateExpression { env with genVarCounter = List.length lambda.pars + .genVarCounter } @state lambda.body

                wrappedBody & parameters =
                    baseBody & []
                    >> List.forReversed __ lambda.pars fn taPar, bodyAcc & eaParsAcc:
                        bodyX & eaPar =
                            translateParameter { env with genVarCounter = List.length eaParsAcc + .genVarCounter } bodyAcc taPar

                        bodyX & [ eaPar, eaParsAcc... ]

                tUsr =
                    EA.translateUsr usr index

                context =
                    try Hash.get @state.lambdaContextes tUsr as
                        'nothing: todo "Missing lambda context"
                        'just c: c

                eaDef as EA.GlobalDefinition =
                    {
                    , context
                    , deps = Dict.empty
                    , expr = wrappedBody
                    , freeTyvars = def.freeTyvars
                    , freeUnivars = def.freeUnivars
                    , parameters
                    , returnType = lambda.returnType
                    , usr = tUsr
                    }

                [ eaDef, defs... ]
