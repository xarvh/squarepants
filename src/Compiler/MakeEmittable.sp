

alias ByName a =
    Dict Name a


# TODO rename this to something that makes sense also outside this module?
alias State = {
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

userSpecifiedName as Text: DollarName =
    name:
    DollarName ("$" .. name)


generatedName as Text: DollarName =
    base:
    DollarName ("$$" .. base)


generateTryName as Int@: DollarName =
    counter@:
    # Using the counter is necessary to avoid shadowing declarations in nested try..as
    @counter += 1
    generatedName ("try" .. Text.fromNumber counter)


translateSource as State@: Meta.Source: Text =
    state@: src:

    # TODO just generate an id for each source, to avoid all stupid collisions and invalid characters
    try src as
        Meta.Core:
            "core"

        Meta.Posix:
            "posix"

        Meta.Browser:
            "browser"

        Meta.SourceDir path:
            try Hash.get state.sourceDirsToId path as
                Nothing:
                    n = "sd" .. Text.fromNumber state.sourceDirsCounter
                    @state.sourceDirsCounter += 1
                    Hash.insert @state.sourceDirsToId path n
                    n

                Just id:
                    id


makeTextUsr as State@: UMR: DollarName: Name =
    state@: umr: (DollarName name):

    UMR source modulePath =
        umr

    "$" .. translateSource @state source .. "$" .. Text.replace "/" "$" modulePath .. name


translateUsr as State@: USR: Text =
    state@: usr:
    USR umr name = usr
    makeTextUsr @state umr (userSpecifiedName name)


#
# Translation
#

union PickedName =
    , TrivialPattern DollarName
    , SafeMainName DollarName
    , NoNamedVariables


pickMainName as Pattern: PickedName =
    pattern:
    try pattern as

        TA.PatternAny pos { isUnique = _, maybeName = Just name, maybeAnnotation = _, type = _ }:
            TrivialPattern (userSpecifiedName name)

        _:
            try Dict.keys (TA.patternNames pattern) as
                head :: tail:
                    SafeMainName (generatedName head)

                []:
                    NoNamedVariables


translatePattern as Pattern: EA.Expression: [ Bool & DollarName & EA.Expression ] =
    pattern: accessExpr:

    translatePatternRec pattern accessExpr []
# TODO we can do this optimization only once we can replace the pattern variable name with the accessExpr variable in the block that uses it
#    try pattern as
#        TA.PatternAny _ _ _: []
#        _: translatePatternRec pattern accessExpr []


translatePatternRec as Pattern: EA.Expression: [ Bool & DollarName & EA.Expression ]: [ Bool & DollarName & EA.Expression ] =
    pattern: accessExpr: accum:
    try pattern as
        TA.PatternAny _ { isUnique, maybeName = Nothing, maybeAnnotation, type }:
            accum

        TA.PatternAny _ { isUnique, maybeName = Just name, maybeAnnotation, type }:
            (isUnique & userSpecifiedName name & accessExpr) :: accum

        TA.PatternLiteralNumber _ _:
            accum

        TA.PatternLiteralText _ _:
            accum

        TA.PatternConstructor _ path pas:
            accum >> List.indexedFor pas index: pa:
                translatePatternRec pa (EA.ConstructorAccess index accessExpr)

        TA.PatternRecord _ attrs:
            accum >> Dict.for attrs name: (pa & type):
                translatePatternRec pa (EA.RecordAccess name accessExpr)


translateVariableArgs as State@: TA.Ref: EA.Expression =
    state@: ref: #({ ref, attrPath }):

    variableName =
        try ref as
            CA.RefLocal name:
                DollarName n =
                    userSpecifiedName name
                n

            CA.RefGlobal usr:
                translateUsr @state usr

    EA.Variable variableName [] #attrPath
#    >> List.for attrPath attributeName: expr:
#        EA.RecordAccess attributeName expr


testPattern as Pattern: EA.Expression: [EA.Expression]: [EA.Expression] =
    pattern: valueToTest: accum:
    try pattern as

        TA.PatternAny _ _:
            accum

        TA.PatternLiteralText _ text:
            EA.ShallowEqual (EA.LiteralText text) valueToTest :: accum

        TA.PatternLiteralNumber _  num:
            EA.ShallowEqual (EA.LiteralNumber num) valueToTest :: accum

        TA.PatternConstructor _ (USR umr name) pas:
            (EA.IsConstructor name valueToTest :: accum)
            >> List.indexedFor pas index: argPattern:
                testPattern argPattern (EA.ConstructorAccess index valueToTest)

        TA.PatternRecord _ attrs:
            accum >> Dict.for attrs name: (pa & type):
                testPattern pa (EA.RecordAccess name valueToTest)


translateExpression as State@: Int@: Expression: EA.Expression =
    state@: counter@: expression:

    try expression as
        TA.LiteralNumber _ num:
            EA.LiteralNumber num

        TA.LiteralText _ text:
            EA.LiteralText text

        TA.Variable _ ref:
            translateVariableArgs @state ref

        TA.Constructor _ usr:
            EA.Constructor (translateUsr @state usr)

        TA.Fn pos args body:
            todo "TA.Fn"
#            try pickMainName pattern as
#                NoNamedVariables:
#                    EA.Lambda Nothing (translateExpression @state @counter body)
#
#                TrivialPattern (DollarName argName):
#                    EA.Lambda (Just argName) (translateExpression @state @counter body)
#
#                SafeMainName (DollarName mainName):
#                    namesAndExpressions =
#                         translatePattern pattern (EA.Variable mainName [])
#
#                    wrapWithArgumentLetIn =
#                        (isUnique & DollarName varName & letExpression): inExpression:
#                        EA.LetIn {
#                            , maybeName = Just varName
#                            #, isUnique
#                            , letExpression
#                            , inExpression
#                            }
#
#                    body
#                    >> translateExpression @state @counter
#                    >> List.for namesAndExpressions wrapWithArgumentLetIn
#                    >> EA.Lambda (Just mainName)

        TA.Record _ extends attrs:
            attrs
            >> Dict.toList
            >> List.sortBy Tuple.first
            >> List.map (Tuple.mapSecond (translateExpression @state @counter))
            >> EA.LiteralRecord (Maybe.map (translateExpression @state @counter) extends)

        TA.Call _ ref args:
            todo "TA.Call"

#            arg =
#                translateVariableArgs @state var
#                >> List.for attrPath attributeName: expr:
#                    EA.RecordAccess attributeName expr
#
#            EA.Call (translateExpression @state @counter ref) [args & True]


        TA.If _ ar:
            EA.Conditional
                (translateExpression @state @counter ar.condition)
                (translateExpression @state @counter ar.true)
                (translateExpression @state @counter ar.false)

        TA.Try pos { value, type, patternsAndExpressions }:

            # 1. create a name for the value (unless it's already a variable)
            valueExpression & wrapWithLetIn =
                try value as
                    TA.Variable _ ref:
                        translateVariableArgs @state ref & identity
                    _:
                        DollarName tryName =
                            generateTryName @counter

                        wrap =
                            tryExpression:
                            EA.LetIn {
                                , maybeName = Just tryName
                                #, isUnique = False
                                , letExpression = translateExpression @state @counter value
                                , inExpression = tryExpression
                                }

                        EA.Variable tryName [] & wrap


            # 2. if-elses
            addTryPatternAndBlock as (Pattern & Expression): EA.Expression: EA.Expression =
                ( pattern & block ): nextTryExpression:

                testIfPatternMatches as EA.Expression =
                    testPattern pattern valueExpression []
                    >> List.reverse
                    >> EA.And

                namesAndExpressions as [ Bool & DollarName & EA.Expression ] =
                    translatePattern pattern valueExpression

                whenConditionMatches as EA.Expression =
                    translateExpression @state @counter block
                    >> List.for namesAndExpressions (isUnique & DollarName name & letExpression): inExpression:
                        EA.LetIn { maybeName = Just name, [#isUnique,#] letExpression, inExpression }

                EA.Conditional testIfPatternMatches whenConditionMatches nextTryExpression

            default =
                EA.MissingPattern pos valueExpression

            default
            >> List.forReversed patternsAndExpressions addTryPatternAndBlock
            >> wrapWithLetIn


        TA.LetIn valueDef e:
            try pickMainName valueDef.pattern as
                NoNamedVariables:
                    EA.LetIn {
                        , maybeName = Nothing
                        #, isUnique = TA.patternContainsUnique valueDef.pattern
                        , letExpression = translateExpression @state @counter valueDef.body
                        , inExpression = translateExpression @state @counter e
                        }

                TrivialPattern (DollarName defName):
                    EA.LetIn
                        {
                        , maybeName = Just defName
                        #, isUnique = TA.patternContainsUnique valueDef.pattern
                        , letExpression = translateExpression @state @counter valueDef.body
                        , inExpression = translateExpression @state @counter e
                        }

                SafeMainName (DollarName mainName):
                    namesAndExpressions =
                        translatePattern valueDef.pattern (EA.Variable mainName [])

                    wrapWithUnpackedPatternVar as (Bool & DollarName & EA.Expression): EA.Expression: EA.Expression =
                        (isUnique & DollarName name & letExpression): inExpression:
                        EA.LetIn {
                            , maybeName = Just name
                            #, isUnique
                            , letExpression
                            , inExpression
                            }

                    wrapWithActualLetIn as EA.Expression: EA.Expression =
                        inExpression:
                        EA.LetIn {
                            , maybeName = Just mainName
                            #, isUnique = False
                            , letExpression = translateExpression @state @counter valueDef.body
                            , inExpression
                            }

                    translateExpression @state @counter e
                    >> List.forReversed namesAndExpressions wrapWithUnpackedPatternVar
                    >> wrapWithActualLetIn


translateRootValueDef as State@: UMR: TA.ValueDef: ByName EA.GlobalDefinition: ByName EA.GlobalDefinition =
    state@: umr: def: accum:

    counter @= 0

    deps =
        Set.map (translateUsr @state) def.directValueDeps

    try pickMainName def.pattern as

        NoNamedVariables:
            accum

        TrivialPattern name:
            usrAsText =
                makeTextUsr @state umr name

            accum >> Dict.insert usrAsText {
              , name = usrAsText
              , expr = translateExpression @state @counter def.body
              , deps
              }

        SafeMainName mainName:
            mainUsrAsText =
                makeTextUsr @state umr mainName

            mainDef as EA.GlobalDefinition =
                { name = mainUsrAsText
                , expr = translateExpression @state @counter def.body
                , deps
                }

            accum
            >> Dict.insert mainUsrAsText mainDef
            >> List.for (translatePattern def.pattern (EA.Variable mainUsrAsText [])) (isUnique & name & expr):
                textUsr = makeTextUsr @state umr name
                Dict.insert textUsr { name = textUsr, expr, deps = Set.singleton mainUsrAsText }


#
# Main
#


circularIsError as ByName EA.GlobalDefinition: [Name]: Bool =
    globalDefsByName: names:

    names >> List.any name:
      try Dict.get name globalDefsByName as
          Nothing:
              # native or some special stuff?
              False

          Just globalDef:
              try globalDef.expr as
                  EA.Lambda _ _:
                      False
                  _:
                      True



translateAll as [TA.Module]: Result [[Name]] (State & [EA.GlobalDefinition]) =
    modules:

    Debug.benchStart None

    state as State @= {
        , sourceDirsToId = Hash.empty
        , sourceDirsCounter = 0
        }

    globalDefsByName as ByName EA.GlobalDefinition =
        Dict.empty >> List.for modules module:
            Dict.for module.valueDefs _: def:
                translateRootValueDef @state module.umr def

    circulars & reorderedNames =
        RefHierarchy.reorder (globalDef: globalDef.deps) globalDefsByName

    Debug.benchStop "makeEmittable"

    errors =
        circulars >> List.filter (circularIsError globalDefsByName)

    if errors /= [] then
        Err errors
    else
       reorderedNames
       >> List.filterMap (name: Dict.get name globalDefsByName)
       >> Tuple.pair state
       >> Ok

