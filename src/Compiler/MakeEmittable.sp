

alias ByName a =
    Dict Name a


# TODO rename this to something that makes sense also outside this module?
alias State =
    {
    , sourceDirsToId as Hash Text Text
    , sourceDirsCounter as Int
    }


alias Expression = TA.Expression
alias Pattern = TA.Pattern


#
# Names
#
# * Names must use only the characters allowed by Squarepants, plus `$`
# * Names are guaranteed to be unique in their scope, no shadowing happens
#
# In order to guarantee uniqueness:
#
# * All names specified by the user start with `$`
# * All names generated by the compiler start with `$$`
#
# (For USRs, these two rules are applied to the `name` part, not to the full name).
#

union DollarName =
    DollarName Text

userSpecifiedName as fn Text: DollarName =
    fn name:
    DollarName ("$" .. name)


generatedName as fn Text: DollarName =
    fn base:
    DollarName ("$$" .. base)


generateTryName as fn @Int: DollarName =
    fn @counter:
    # Using the counter is necessary to avoid shadowing declarations in nested try..as
    @counter += 1
    generatedName ("try" .. Text.fromNumber (cloneUni @counter))


translateSource as fn @State, Meta.Source: Text =
    fn @state, src:

    # TODO just generate an id for each source, to avoid all stupid collisions and invalid characters
    try src as
        , Meta.Core:
            "core"

        , Meta.Posix:
            "posix"

        , Meta.Browser:
            "browser"

        , Meta.SourceDir path:
            try Hash.get @state.sourceDirsToId path as
                , Nothing:
                    n = "sd" .. Text.fromNumber (cloneUni @state.sourceDirsCounter)
                    @state.sourceDirsCounter += 1
                    Hash.insert @state.sourceDirsToId path n
                    n

                , Just id:
                    id


makeTextUsr as fn @State, UMR, DollarName: Name =
    fn @state, umr, (DollarName name):

    UMR source modulePath =
        umr

    "$" .. translateSource @state source .. "$" .. Text.replace "/" "$" modulePath .. name


translateUsr as fn @State, USR: Text =
    fn @state, usr:
    USR umr name = usr
    makeTextUsr @state umr (userSpecifiedName name)


#
# Translation
#

union PickedName =
    , TrivialPattern DollarName TA.FullType
    , SafeMainName DollarName
    , NoNamedVariables


pickMainName as fn Pattern: PickedName =
    fn pattern:
    try pattern as

        , TA.PatternAny pos { maybeName = Just name, type }:
            TrivialPattern (userSpecifiedName name) type

        , _:
            try Dict.keys (TA.patternNames pattern) as
                , head :: tail:
                    SafeMainName (generatedName head)

                , []:
                    NoNamedVariables


translatePattern as fn Pattern, EA.Expression: [ TA.FullType & DollarName & EA.Expression ] =
    fn pattern, accessExpr:

    translatePatternRec pattern accessExpr []
# TODO we can do this optimization only once we can replace the pattern variable name with the accessExpr variable in the block that uses it
#    try pattern as
#        TA.PatternAny _ _ _: []
#        _: translatePatternRec pattern accessExpr []


translatePatternRec as fn Pattern, EA.Expression, [ TA.FullType & DollarName & EA.Expression ]: [ TA.FullType & DollarName & EA.Expression ] =
    fn pattern, accessExpr, accum:
    try pattern as
        , TA.PatternAny _ { maybeName = Nothing, type }:
            accum

        , TA.PatternAny _ { maybeName = Just name, type }:
            (type & userSpecifiedName name & accessExpr) :: accum

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


translateVariableArgs as fn @State, Ref: EA.Expression =
    fn @state, ref: #({ ref, attrPath }):

    variableName =
        try ref as
            , RefLocal name:
                DollarName n =
                    userSpecifiedName name
                n

            , RefGlobal usr:
                translateUsr @state usr

    EA.Variable variableName #attrPath
#    >> List.for attrPath attributeName: expr:
#        EA.RecordAccess attributeName expr


testPattern as fn Pattern, EA.Expression, [EA.Expression]: [EA.Expression] =
    fn pattern, valueToTest, accum:
    try pattern as

        , TA.PatternAny _ _:
            accum

        , TA.PatternLiteralText _ text:
            EA.ShallowEqual (EA.LiteralText text) valueToTest :: accum

        , TA.PatternLiteralNumber _  num:
            EA.ShallowEqual (EA.LiteralNumber num) valueToTest :: accum

        , TA.PatternConstructor _ (USR umr name) pas:
            (EA.IsConstructor name valueToTest :: accum)
            >> List.indexedFor __ pas fn index, argPattern, a:
                testPattern argPattern (EA.ConstructorAccess index valueToTest) a

        , TA.PatternRecord _ attrs:
            accum >> Dict.for __ attrs fn name, (pa & type), a:
                testPattern pa (EA.RecordAccess name valueToTest) a



translateParameter as fn @State, @Int, EA.Expression, TA.Parameter: EA.Expression & (Bool & Maybe Name) =
    fn @state, @counter, bodyAcc, param:

    try param as
        , TA.ParameterRecycle pos rawType name:

            DollarName n =
                userSpecifiedName name

            bodyAcc & (True & Just n)

        , TA.ParameterPattern fullType pa:
            try pickMainName pa as
                , NoNamedVariables:
                    bodyAcc & (False & Nothing)

                , TrivialPattern (DollarName argName) type:
                    bodyAcc & (False & Just argName)

                , SafeMainName (DollarName mainName):
                    namesAndExpressions =
                         translatePattern pa (EA.Variable mainName)

                    wrapWithArgumentLetIn =
                        fn (type & DollarName varName & letExpression), inExpression:
                        EA.LetIn
                            {
                            , maybeName = Just varName
                            , letExpression
                            , inExpression
                            , type
                            }

                    List.for bodyAcc namesAndExpressions wrapWithArgumentLetIn & (False & Just mainName)


translateArgAndType as fn @State, @Int, TA.Argument: EA.Argument =
    fn @state, @counter, taArg:

    try taArg as
        , TA.ArgumentExpression fullType exp:
            EA.ArgumentSpend fullType (translateExpression @state @counter exp)

        , TA.ArgumentRecycle pos rawType attrPath name:

            DollarName n =
                userSpecifiedName name

            EA.ArgumentRecycle rawType attrPath n


translateExpression as fn @State, @Int, Expression: EA.Expression =
    fn @state, @counter, expression:

    try expression as
        , TA.LiteralNumber _ num:
            EA.LiteralNumber num

        , TA.LiteralText _ text:
            EA.LiteralText text

        , TA.Variable _ ref:
            translateVariableArgs @state ref

        , TA.Constructor _ usr:
            EA.Constructor (translateUsr @state usr)

        , TA.RecordAccess _ attrName exp:
            EA.RecordAccess attrName (translateExpression @state @counter exp)

        , TA.Fn pos taPars body bodyT:

            eaBody =
                translateExpression @state @counter body

            wrappedBody & eaPars =
                eaBody & []
                >> List.forReversed __ taPars fn taPar, (bodyAcc & eaParsAcc):
                    bodyX & eaPar =
                        translateParameter @state @counter bodyAcc taPar
                    bodyX & (eaPar :: eaParsAcc)

            EA.Fn eaPars wrappedBody

        , TA.Record _ extends attrs:
            attrs
            >> Dict.toList
            >> List.sortBy Tuple.first __
            >> List.map (Tuple.mapSecond (translateExpression @state @counter __) __) __
            >> EA.LiteralRecord (Maybe.map (translateExpression @state @counter __) extends) __

        , TA.Call _ ref argsAndTypes:
            EA.Call
                (translateExpression @state @counter ref)
                (List.map (translateArgAndType @state @counter __) argsAndTypes)

        , TA.If _ ar:
            EA.Conditional
                (translateExpression @state @counter ar.condition)
                (translateExpression @state @counter ar.true)
                (translateExpression @state @counter ar.false)

        , TA.Try pos { value, valueType, patternsAndExpressions }:

            # 1. create a name for the value (unless it's already a variable)
            valueExpression & wrapWithLetIn =
                try value & valueType.uni as
                    , TA.Variable _ ref & Imm:
                        translateVariableArgs @state ref & identity

                    , _:
                        DollarName tryName =
                            generateTryName @counter

                        wrap =
                            fn tryExpression:
                            EA.LetIn
                                {
                                , maybeName = Just tryName
                                , letExpression = translateExpression @state @counter value
                                , inExpression = tryExpression
                                , type = valueType
                                }

                        EA.Variable tryName & wrap


            # 2. if-elses
            addTryPatternAndBlock as fn (Pattern & Expression), EA.Expression: EA.Expression =
                fn ( pattern & block ), nextTryExpression:

                testIfPatternMatches as EA.Expression =
                    testPattern pattern valueExpression []
                    >> List.reverse
                    >> EA.And

                namesAndExpressions as [ TA.FullType & DollarName & EA.Expression ] =
                    translatePattern pattern valueExpression

                whenConditionMatches as EA.Expression =
                    translateExpression @state @counter block
                    >> List.for __ namesAndExpressions fn (type & DollarName name & letExpression), inExpression:
                        EA.LetIn { maybeName = Just name, type, letExpression, inExpression }

                EA.Conditional testIfPatternMatches whenConditionMatches nextTryExpression

            default =
                EA.MissingPattern pos valueExpression

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
                        , letExpression = translateExpression @state @counter valueDef.body
                        , inExpression = translateExpression @state @counter e
                        }

                , TrivialPattern (DollarName defName) type:
                    EA.LetIn
                        {
                        , maybeName = Just defName
                        , type
                        , letExpression = translateExpression @state @counter valueDef.body
                        , inExpression = translateExpression @state @counter e
                        }

                , SafeMainName (DollarName mainName):
                    namesAndExpressions =
                        translatePattern valueDef.pattern (EA.Variable mainName)

                    wrapWithUnpackedPatternVar as fn (TA.FullType & DollarName & EA.Expression), EA.Expression: EA.Expression =
                        fn (type & DollarName name & letExpression), inExpression:
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
                            , letExpression = translateExpression @state @counter valueDef.body
                            , inExpression
                            }

                    translateExpression @state @counter e
                    >> List.forReversed __ namesAndExpressions wrapWithUnpackedPatternVar
                    >> wrapWithActualLetIn

        , TA.DestroyIn name e:
            translateExpression @state @counter e



translateRootValueDef as fn @State, UMR, TA.ValueDef, ByName EA.GlobalDefinition: ByName EA.GlobalDefinition =
    fn @state, umr, def, accum:

    !counter = 0

    deps =
        Set.map (translateUsr @state __) def.directValueDeps

    try pickMainName def.pattern as

        , NoNamedVariables:
            accum

        , TrivialPattern name type:
            usrAsText =
                makeTextUsr @state umr name

            accum >> Dict.insert usrAsText {
              , name = usrAsText
              , expr = translateExpression @state @counter def.body
              , deps
              }
              __

        , SafeMainName mainName:
            mainUsrAsText =
                makeTextUsr @state umr mainName

            mainDef as EA.GlobalDefinition =
                { name = mainUsrAsText
                , expr = translateExpression @state @counter def.body
                , deps
                }

            accum
            >> Dict.insert mainUsrAsText mainDef __
            >> List.for __ (translatePattern def.pattern (EA.Variable mainUsrAsText)) fn (type & name & expr), z:
                textUsr = makeTextUsr @state umr name
                Dict.insert textUsr { name = textUsr, expr, deps = Set.ofOne mainUsrAsText } z


#
# Main
#


circularIsError as fn ByName EA.GlobalDefinition, [Name]: Bool =
    fn globalDefsByName, names:

    zzz = fn name:
      try Dict.get name globalDefsByName as
          , Nothing:
              # native or some special stuff?
              False

          , Just globalDef:
              try globalDef.expr as
                  , EA.Fn _ _:
                      False
                  , _:
                      True

    List.any zzz names


translateAll as fn [TA.Module]: Result [[Name]] (State & [EA.GlobalDefinition]) =
    fn modules:

    Debug.benchStart None

    !state as State =
        {
        , sourceDirsToId = Hash.fromList []
        , sourceDirsCounter = 0
        }

    globalDefsByName as ByName EA.GlobalDefinition =
        Dict.empty >> List.for __ modules fn module, d:
            Dict.for d module.valueDefs fn _, def, a:
                translateRootValueDef @state module.umr def a

    circulars & reorderedNames =
        RefHierarchy.reorder (fn globalDef: globalDef.deps) globalDefsByName

    Debug.benchStop "makeEmittable"

    errors =
        circulars >> List.filter (circularIsError globalDefsByName __) __

    s = state

    if errors /= [] then
        Err errors
    else
       reorderedNames
       >> List.filterMap (fn name: Dict.get name globalDefsByName) __
       >> Tuple.pair s __
       >> Ok

