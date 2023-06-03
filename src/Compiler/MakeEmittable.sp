

alias Env =
    {
    , genVarCounter as Int
    , module as TA.Module
    }


#
# Translation
#


union PickedName =
    , TrivialPattern Name TA.FullType
    , GenerateName
    , NoNamedVariables


generateName as fn Env: Name & Env =
    fn env:
    Text.fromNumber (env.genVarCounter + 1) & { env with genVarCounter = 1 + .genVarCounter }


pickMainName as fn TA.Pattern: PickedName =
    fn pattern:
    try pattern as

        , TA.PatternAny pos { maybeName = Just name, type }:
            TrivialPattern name type

        , _:
            if TA.patternNames pattern /= Dict.empty then
                GenerateName
            else
                NoNamedVariables


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
        , TA.PatternAny _ { maybeName = Nothing, type }:
            accum

        , TA.PatternAny _ { maybeName = Just name, type }:
            (type & name & accessExpr) :: accum

        , TA.PatternLiteralNumber _ _:
            accum

        , TA.PatternLiteralText _ _:
            accum

        , TA.PatternConstructor _ path pas:
            accum >> List.indexedFor __ pas fn index, pa, a:
                translatePatternRec pa (EA.ConstructorAccess index accessExpr) a

        , TA.PatternRecord _ attrs:
            accum >> Dict.for __ attrs fn name, (pa & type), a:
                translatePatternRec pa (EA.RecordAccess name accessExpr) a


testPattern as fn TA.Pattern, EA.Expression, [EA.Expression]: [EA.Expression] =
    fn pattern, valueToTest, accum:
    try pattern as

        , TA.PatternAny _ _:
            accum

        , TA.PatternLiteralText _ text:
            EA.CompareWithLiteralText text valueToTest :: accum

        , TA.PatternLiteralNumber _  num:
            EA.CompareWithLiteralNumber num valueToTest :: accum

        , TA.PatternConstructor _ (USR umr name) pas:
            (EA.IsConstructor name valueToTest :: accum)
            >> List.indexedFor __ pas fn index, argPattern, a:
                testPattern argPattern (EA.ConstructorAccess index valueToTest) a

        , TA.PatternRecord _ attrs:
            accum >> Dict.for __ attrs fn name, (pa & type), a:
                testPattern pa (EA.RecordAccess name valueToTest) a



translateParameter as fn Env, EA.Expression, TA.Parameter: EA.Expression & (Bool & Maybe Name) =
    fn env, bodyAcc, param:

    try param as
        , TA.ParameterRecycle pos rawType name:
            bodyAcc & (True & Just name)

        , TA.ParameterPattern fullType pa:
            try pickMainName pa as
                , NoNamedVariables:
                    bodyAcc & (False & Nothing)

                , TrivialPattern argName type:
                    bodyAcc & (False & Just argName)

                , GenerateName:
                    mainName & newEnv =
                        generateName env

                    namesAndExpressions =
                         translatePattern pa (EA.Variable (RefLocal mainName))

                    wrapWithArgumentLetIn =
                        fn (type & varName & letExpression), inExpression:
                        EA.LetIn
                            {
                            , maybeName = Just varName
                            , letExpression
                            , inExpression
                            , type
                            }

                    List.for bodyAcc namesAndExpressions wrapWithArgumentLetIn & (False & Just mainName)


translateArgAndType as fn Env, TA.Argument: EA.Argument =
    fn env, taArg:

    try taArg as
        , TA.ArgumentExpression fullType exp:
            EA.ArgumentSpend fullType (translateExpression env exp)

        , TA.ArgumentRecycle pos rawType attrPath name:
            EA.ArgumentRecycle rawType attrPath name


[#

    TODO: Explain why are we doing this batshit mess


#]
alias TransOut =
    {
    , recByStats as Set Name
    , stats as [EA.Statement]
    , recByExpr as Set Name
    , expr as EA.Expression
    }


none as fn EA.Expression: TransOut =
    fn expr:
    {
    , recByStats = Set.empty
    , stats as []
    , recByExpr = Set.empty
    , expr
    }



translateExpression as fn Env, TA.Expression: TransOut =
    fn env, expression:

    try expression as
        , TA.LiteralNumber _ num:
            none << EA.LiteralNumber num

        , TA.LiteralText _ text:
            none << EA.LiteralText text

        , TA.Variable _ ref:
            none << EA.Variable ref

        , TA.Constructor _ usr:
            none << EA.Constructor usr

        , TA.RecordAccess _ attrName exp:
            { translateExpression env exp with expr = EA.RecordAccess attrName .expr }

        , TA.Fn pos taPars body bodyT:
            bodyOut =
                translateExpression { env with genVarCounter = List.length taPars + .genVarCounter } body

            eaBody =
                # TODO use EA.Evaluation instead of EA.Return if bodyT is `None`?
                List.concat [ bodyOut.stats, [ EA.Return bodyOut.expr ] ]




            # TODO: add another acc to the for below, remove any rec in pas, starting from eaBody.recByStats + eaBody.recByExpr
            recByExpr =
                all bodyOut.recs - all recyclable pars

            wrappedBody & eaPars =
                eaBody & []
                >> List.forReversed __ taPars fn taPar, (bodyAcc & eaParsAcc):
                    bodyX & eaPar =
                        newEnv = { env with genVarCounter = List.length eaParsAcc + .genVarCounter }
                        translateParameter newEnv bodyAcc taPar
                    bodyX & (eaPar :: eaParsAcc)



            { recByStats = Set.none, stats = [], recByExpr, expr = EA.Fn eaPars wrappedBody }

        , TA.Record _ extends attrs:
            attrs
            >> Dict.toList
            >> List.sortBy Tuple.first __


            ----> Ensure all side effecty ops are done in order!!!


            >> List.map (Tuple.mapSecond (translateExpression env __) __) __
            >> EA.LiteralRecord (Maybe.map (translateExpression env __) extends) __

        , TA.Call _ ref argsAndTypes:

            refStats & refExpr =
                translateExpression env ref

            argStats



            EA.Call
                (translateExpression env ref)

                ----> Ensure all side effecty ops are done in order!!!
                (List.map (translateArgAndType env __) argsAndTypes)

        , TA.If _ ar:
            EA.Conditional
                (translateExpression env ar.condition)
                (translateExpression env ar.true)
                (translateExpression env ar.false)

        , TA.Try pos { value, valueType, patternsAndExpressions }:

            # 1. create a name for the value (unless it's already a variable)
            valueExpression & wrapWithLetIn & newEnv =
                try value & valueType.uni as
                    , TA.Variable _ ref & Imm:
                        EA.Variable ref & identity & env

                    , _:
                        tryName & newEnv =
                            generateName env

                        wrap =
                            fn tryExpression:
                            EA.LetIn
                                {
                                , maybeName = Just tryName
                                , letExpression = translateExpression newEnv value
                                , inExpression = tryExpression
                                , type = valueType
                                }

                        EA.Variable (RefLocal tryName) & wrap & newEnv


            # 2. if-elses
            addTryPatternAndBlock as fn (TA.Pattern & TA.Expression), EA.Expression: EA.Expression =
                fn ( pattern & block ), nextTryExpression:

                testIfPatternMatches as EA.Expression =
                    testPattern pattern valueExpression []
                    >> List.reverse
                    >> EA.PatternMatchConditions

                namesAndExpressions as [ TA.FullType & Name & EA.Expression ] =
                    translatePattern pattern valueExpression

                whenConditionMatches as EA.Expression =
                    translateExpression newEnv block
                    >> List.for __ namesAndExpressions fn (type & name & letExpression), inExpression:
                        EA.LetIn { maybeName = Just name, type, letExpression, inExpression }

                EA.Conditional testIfPatternMatches whenConditionMatches nextTryExpression

            default =
                human = Error.posToHuman { fsPath = env.module.fsPath, content = env.module.asText } pos
                EA.MissingPattern human.location valueExpression

            default
            >> List.forReversed __ patternsAndExpressions addTryPatternAndBlock
            >> wrapWithLetIn


        , TA.LetIn valueDef e bodyType:
            try pickMainName valueDef.pattern as
                , NoNamedVariables:
                    EA.LetIn
                        {
                        , maybeName = Nothing
                        , type = valueDef.type
                        , letExpression = translateExpression env valueDef.body
                        , inExpression = translateExpression env e
                        }

                , TrivialPattern defName type:
                    EA.LetIn
                        {
                        , maybeName = Just defName
                        , type
                        , letExpression = translateExpression env valueDef.body
                        , inExpression = translateExpression env e
                        }

                , GenerateName:
                    mainName & newEnv =
                        # TODO check if valueDef.body is just a variable
                        generateName env

                    namesAndExpressions =
                        translatePattern valueDef.pattern (EA.Variable (RefLocal mainName))

                    wrapWithUnpackedPatternVar as fn (TA.FullType & Name & EA.Expression), EA.Expression: EA.Expression =
                        fn (type & name & letExpression), inExpression:
                        EA.LetIn
                            {
                            , maybeName = Just name
                            , type
                            , letExpression
                            , inExpression
                            }

                    wrapWithActualLetIn as fn EA.Expression: EA.Expression =
                        fn inExpression:
                        EA.LetIn
                            {
                            , maybeName = Just mainName
                            , type = valueDef.type
                            , letExpression = translateExpression newEnv valueDef.body
                            , inExpression
                            }

                    translateExpression newEnv e
                    >> List.forReversed __ namesAndExpressions wrapWithUnpackedPatternVar
                    >> wrapWithActualLetIn

        , TA.DestroyIn name e:
            translateExpression env e



translateRootValueDef as fn Env, TA.ValueDef, ByUsr EA.GlobalDefinition: ByUsr EA.GlobalDefinition =
    fn env, def, accum:

    deps =
        def.directValueDeps

    try pickMainName def.pattern as

        , NoNamedVariables:
            accum

        , TrivialPattern name type:
            usr =
                USR env.module.umr name

            Dict.insert usr
                {
                , usr
                , expr = translateExpression env def.body
                , deps
                }
                accum

        , GenerateName:
            mainName & newEnv =
                generateName env

            mainUsr =
                USR env.module.umr mainName

            mainDef as EA.GlobalDefinition =
                {
                , usr = mainUsr
                , expr = translateExpression newEnv def.body
                , deps
                }

            accum
            >> Dict.insert mainUsr mainDef __
            >> List.for __ (translatePattern def.pattern (EA.Variable (RefGlobal mainUsr))) fn (type & name & expr), z:
                subUsr = USR env.module.umr name
                Dict.insert subUsr { usr = subUsr, expr, deps = Set.ofOne mainUsr } z


#
# Main
#


circularIsError as fn ByUsr EA.GlobalDefinition, [USR]: Bool =
    fn globalDefsByName, usrs:

    zzz =
      fn usr:
      try Dict.get usr globalDefsByName as
          , Nothing:
              # native or some special stuff?
              False

          , Just globalDef:
              try globalDef.expr as
                  , EA.Fn _ _: False
                  , _: True

    List.any zzz usrs


circularToError as fn [USR]: Error =
    fn usrs:

    Error.Raw
        [ "circular dependency: "
        , ... List.map toHuman usrs
        ]


translateAll as fn UMR, [TA.Module]: Res { entryUsr as USR, defs as [EA.GlobalDefinition] } =
    fn entryModule, modules:

    Debug.benchStart None

    globalDefsByName as ByUsr EA.GlobalDefinition =
        Dict.empty >> List.for __ modules fn module, d:
            Dict.for d module.valueDefs fn _, def, a:
                env = { genVarCounter = 0, module }
                translateRootValueDef env def a

    circulars & reorderedNames =
        RefHierarchy.reorder (fn globalDef: globalDef.deps) globalDefsByName

    Debug.benchStop "makeEmittable"

    errors =
        List.filter (circularIsError globalDefsByName __) circulars

    if errors /= [] then
        errors
        >> List.map circularToError __
        >> Error.Nested
        >> Err

    else
        {
        , defs = List.filterMap (fn name: Dict.get name globalDefsByName) reorderedNames
        , entryUsr = USR entryModule "main"
        }
        >> Ok
