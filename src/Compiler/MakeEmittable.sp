

alias ByName a =
    Dict Name a


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


translateSource as Meta.Source: Text =
    src:

    # TODO just generate an id for each source, to avoid all stupid collisions and invalid characters
    try src as
        Meta.Core:
            "core"

        Meta.Posix:
            "posix"

        Meta.Browser:
            "browser"

        Meta.SourceDir path:
            if Text.startsWithRegex "[a-zA-Z0-9_./]*$" path == "" then
                todo << "Invalid chars in source dir name: " .. path
            else
                path
                    >> Text.replace "." "_"
                    >> Text.replace "/" "$"


makeTextUsr as Meta.UniqueModuleReference: DollarName: Name =
    umr: (DollarName name):

    Meta.UMR source modulePath =
        umr

    "$" .. translateSource source .. "$" .. Text.replace "/" "$" modulePath .. name


translateUsr as Meta.UniqueSymbolReference: Text =
    usr:
    Meta.USR umr name = usr
    makeTextUsr umr (userSpecifiedName name)


#
# Translation
#

union PickedName =
    , TrivialPattern DollarName
    , SafeMainName DollarName
    , NoNamedVariables


pickMainName as CA.Pattern: PickedName =
    pattern:

    try pattern as
        CA.PatternNamed pos mutability name maybeType:
            TrivialPattern (userSpecifiedName name)

        _:
            try Dict.keys (CA.patternNames pattern) as
                head :: tail:
                    SafeMainName (generatedName head)

                []:
                    NoNamedVariables


translatePattern as CA.Pattern: EA.Expression: [ DollarName & EA.Expression ] =
    pattern: accessExpr:

    translatePatternRec pattern accessExpr []
# TODO we can do this optimization only once we can replace the pattern variable name with the accessExpr variable in the block that uses it
#    try pattern as
#        CA.PatternAny _ _ _: []
#        _: translatePatternRec pattern accessExpr []


translatePatternRec as CA.Pattern: EA.Expression: [ DollarName & EA.Expression ]: [ DollarName & EA.Expression ] =
    pattern: accessExpr: accum:
    try pattern as
        CA.PatternDiscard _ _:
            accum

        CA.PatternNamed _ mutability name _:
            (userSpecifiedName name & accessExpr) :: accum

        CA.PatternLiteralNumber _ _:
            accum

        CA.PatternLiteralText _ _:
            accum

        CA.PatternConstructor _ path pas:
            accum >> List.indexedFor pas index: pa:
                translatePatternRec pa (EA.ConstructorAccess index accessExpr)

        CA.PatternRecord _ attrs:
            accum >> Dict.for attrs name: pa:
                translatePatternRec pa (EA.RecordAccess name accessExpr)


translateVariableArgs as CA.VariableArgs: EA.Expression =
    ({ ref, attrPath }):

    variableName =
        try ref as
            CA.RefBlock name:
                DollarName n =
                    userSpecifiedName name
                n

            CA.RefRoot usr:
                translateUsr usr

    EA.Variable variableName attrPath
#    >> List.for attrPath attributeName: expr:
#        EA.RecordAccess attributeName expr


testPattern as CA.Pattern: EA.Expression: [EA.Expression]: [EA.Expression] =
    pattern: valueToTest: accum:
    try pattern as
        CA.PatternDiscard _ _:
            accum

        CA.PatternNamed _ _ _ _:
            accum

        CA.PatternLiteralText _ text:
            EA.ShallowEqual (EA.LiteralText text) valueToTest :: accum

        CA.PatternLiteralNumber _  num:
            EA.ShallowEqual (EA.LiteralNumber num) valueToTest :: accum

        CA.PatternConstructor _ (Meta.USR umr name) pas:
            (EA.IsConstructor name valueToTest :: accum)
            >> List.indexedFor pas index: argPattern:
                testPattern argPattern (EA.ConstructorAccess index valueToTest)

        CA.PatternRecord _ attrs:
            accum >> Dict.for attrs name: pa:
                testPattern pa (EA.RecordAccess name valueToTest)


translateExpression as Int@: CA.Expression: EA.Expression =
    counter@: expression:

    try expression as
        CA.LiteralNumber _ num:
            EA.LiteralNumber num

        CA.LiteralText _ text:
            EA.LiteralText text

        CA.Variable _ var:
            translateVariableArgs var

        CA.Constructor _ usr:
            EA.Constructor (translateUsr usr)

        CA.Lambda pos (CA.ParameterMutable _ name) body:
            DollarName n = userSpecifiedName name
            EA.Lambda (Just n & EA.Mutable) (translateExpression @counter body)

        CA.Lambda pos (CA.ParameterPattern pattern) body:
            try pickMainName pattern as
                NoNamedVariables:
                    EA.Lambda (Nothing & EA.Immutable) (translateExpression @counter body)

                TrivialPattern (DollarName argName):
                    EA.Lambda (Just argName & EA.Immutable) (translateExpression @counter body)

                SafeMainName (DollarName mainName):
                    namesAndExpressions =
                         translatePattern pattern (EA.Variable mainName [])

                    wrapWithArgumentLetIn =
                        (DollarName varName & letExpression): inExpression:
                        EA.LetIn {
                            , maybeName = Just varName
                            , mutability = EA.Immutable
                            , letExpression
                            , inExpression
                            }

                    body
                    >> translateExpression @counter
                    >> List.for namesAndExpressions wrapWithArgumentLetIn
                    >> EA.Lambda (Just mainName & EA.Immutable)

        CA.Record _ extends attrs:
            attrs
            >> Dict.toList
            >> List.sortBy Tuple.first
            >> List.map (Tuple.mapSecond (translateExpression @counter))
            >> EA.LiteralRecord (Maybe.map translateVariableArgs extends)

        CA.Call _ ref (CA.ArgumentMutable _ var):
            EA.Call (translateExpression @counter ref) (translateVariableArgs var & EA.Mutable)

        CA.Call _ ref (CA.ArgumentExpression expr):
            EA.Call (translateExpression @counter ref) (translateExpression @counter expr & EA.Immutable)

        CA.If _ ar:
            EA.Conditional
                (translateExpression @counter ar.condition)
                (translateExpression @counter ar.true)
                (translateExpression @counter ar.false)

        CA.Try pos value tries:

            # 1. create a name for the value (unless it's already a variable)
            valueExpression & wrapWithLetIn =
                try value as
                    CA.Variable _ { ref, attrPath = [] }:
                        translateVariableArgs { ref, attrPath = [] } & identity
                    _:
                        DollarName tryName =
                            generateTryName @counter

                        wrap =
                            tryExpression:
                            EA.LetIn {
                                , maybeName = Just tryName
                                , mutability = EA.Immutable
                                , letExpression = translateExpression @counter value
                                , inExpression = tryExpression
                                }

                        EA.Variable tryName [] & wrap


            # 2. if-elses
            addTryPatternAndBlock as (CA.Pattern & CA.Expression): EA.Expression: EA.Expression =
                ( pattern & block ): nextTryExpression:

                testIfPatternMatches as EA.Expression =
                    testPattern pattern valueExpression []
                    >> List.reverse
                    >> EA.And

                namesAndExpressions as [ DollarName & EA.Expression ] =
                    translatePattern pattern valueExpression

                whenConditionMatches as EA.Expression =
                    translateExpression @counter block
                    >> List.for namesAndExpressions (DollarName name & letExpression): inExpression:
                        EA.LetIn { maybeName = Just name, mutability = EA.Immutable, letExpression, inExpression }

                EA.Conditional testIfPatternMatches whenConditionMatches nextTryExpression

            default =
                EA.MissingPattern pos valueExpression

            default
            >> List.forReversed tries addTryPatternAndBlock
            >> wrapWithLetIn


        CA.LetIn valueDef e:
            try pickMainName valueDef.pattern as
                NoNamedVariables:
                    EA.LetIn {
                        , maybeName = Nothing
                        , mutability = EA.Immutable
                        , letExpression = translateExpression @counter valueDef.body
                        , inExpression = translateExpression @counter e
                        }

                TrivialPattern (DollarName defName):
                    EA.LetIn {
                        , maybeName = Just defName
                        , mutability = Debug.todo "mutability0"
                        , letExpression = translateExpression @counter valueDef.body
                        , inExpression = translateExpression @counter e
                        }

                SafeMainName (DollarName mainName):
                    namesAndExpressions =
                        translatePattern valueDef.pattern (EA.Variable mainName [])

                    wrapWithUnpackedPatternVar as (DollarName & EA.Expression): EA.Expression: EA.Expression =
                        (DollarName name & letExpression): inExpression:
                        EA.LetIn {
                            , maybeName = Just name
                            , mutability = EA.Immutable
                            , letExpression
                            , inExpression
                            }

                    wrapWithActualLetIn as EA.Expression: EA.Expression =
                        inExpression:
                        EA.LetIn {
                            , maybeName = Just mainName
                            , mutability = EA.Immutable
                            , letExpression = translateExpression @counter valueDef.body
                            , inExpression
                            }

                    translateExpression @counter e
                    >> List.forReversed namesAndExpressions wrapWithUnpackedPatternVar
                    >> wrapWithActualLetIn


translateRootValueDef as Meta.UniqueModuleReference: CA.ValueDef: ByName EA.GlobalDefinition: ByName EA.GlobalDefinition =
    umr: def: accum:

    counter @= 0

    deps =
        Set.map translateUsr def.directValueDeps

    try pickMainName def.pattern as

        NoNamedVariables:
            accum

        TrivialPattern name:
            usrAsText =
                makeTextUsr umr name

            accum >> Dict.insert usrAsText {
              , name = usrAsText
              , expr = translateExpression @counter def.body
              , deps
              }

        SafeMainName mainName:
            mainUsrAsText =
                makeTextUsr umr mainName

            mainDef as EA.GlobalDefinition =
                { name = mainUsrAsText
                , expr = translateExpression @counter def.body
                , deps
                }

            accum
            >> Dict.insert mainUsrAsText mainDef
            >> List.for (translatePattern def.pattern (EA.Variable mainUsrAsText [])) (name & expr):
                textUsr = makeTextUsr umr name
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



translateAll as [CA.Module]: Result [[Name]] [EA.GlobalDefinition] =
    userModules:

    Debug.benchStart None

    modules =
        List.concat [ userModules, Prelude.coreModules ]

    globalDefsByName as ByName EA.GlobalDefinition =
        Dict.empty >> List.for modules module:
            Dict.for module.valueDefs _: def:
                translateRootValueDef module.umr def

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
       >> Ok
