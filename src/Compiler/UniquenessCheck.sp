
alias Argument = CA.Argument
alias Expression = CA.Expression




union MutableAvailability =
    , Available
    , ConsumedAt Pos


union VariableMode =
    , Immutable
    , Mutable MutableAvailability


alias Variable = {
    , definedAt as Pos
    , name as Name
    , mode as VariableMode
    }


alias Env = {
    , variables as Dict Name Variable
    }


alias State = {
    , errors as Array Error
    }




addError as Pos: State@: (Error.Env: List Text): None =
    pos: state@: messageConstructor:

    Array.push @state.errors (Error.Simple pos messageConstructor)


#
# Errors
#

errorReferencingConsumedVariable as Text: Pos: Pos: State@: None =
    name: pos: consumedPos: state@:

    addError pos @state eenv:

        { location, block } =
            Error.posToHuman eenv pos

        [
        , "You can't reference again the variable `" .. name .. "` because it was used already here:"
        , block
        ]


errorUndefinedVariable as Pos: Text: State@: None =
    p: name: state@:

    addError p @state eenv: [
        , "undefined variable: " .. name
        ]







errorMutatingAnImmutable as Text: Pos: State@: None =
    name: p: state@:

    addError p @state eenv: [
        , name .. " is immutable, but you are trying to mutate it"
        ]


errorConsumingRecycledParameter as Text: Pos: State@: None =
    name: pos: state@:

    addError pos @state eenv: [
        , name .. " is passed as recycled, but the function wants to spend it"
        ]


errorMutatingAConsumed as Text: Pos: Pos: State@: None =
    name: p1: p2: state@:

    addError p1 @state eenv:

        { location, block } =
            Error.posToHuman eenv p2

        [
        , "the variable `" .. name .. "` can't be used in this call because it is being consumed here: "
        , block
        ]


errorMutatingTwice as Text: Pos: Pos: State@: None =
    name: p1: p2: state@:

    addError p1 @state eenv:

        { location, block } =
            Error.posToHuman eenv p2

        [
        , name .. " is already being mutated here: "
        , block
        , "You can't use the same mutable twice in the same function call"
        , "TODO: link to wiki explaining why"
        ]





consumeInEnv as Dict Name Pos: Env: Env =
    consumed: env:

    translate =
        name: variable:
            try Dict.get name consumed as
                Nothing: variable
                Just pos: { variable with mode = Mutable << ConsumedAt pos }

    { env with variables = Dict.map translate .variables }


addPatternToEnv as CA.Pattern: Env: Dict Name Pos & Env =
    pattern: env:

    names =
        CA.patternNames pattern

    insertVariable =
       name: stuff:

       mode =
           if stuff.isUnique then Mutable Available else Immutable

       Dict.insert name { definedAt = stuff.pos, name, mode }

    localEnv =
        { env with variables = Dict.for names insertVariable .variables }

    uniques =
        names
        >> Dict.filter (n: s: s.isUnique)
        >> Dict.map (n: s: s.pos)

    uniques & localEnv



#doCall as Env: State@: Pos: Expression: Argument: { mutables as Dict Name Pos, consumed as Dict Name Pos, expression as Expression } =
#    env: state@: pos: reference: argument:
#
#    doneReference =
#        try reference as
#            CA.Call p ref arg:
#                doCall env @state p ref arg
#
#            _:
#              consumed & expression =
#                  doExpression env @state reference
#
#              { mutables = Dict.empty, consumed, expression }
#
#
#    doneArg =
#        doArgument env @state pos {
#            , mutables = doneReference.mutables
#            , consumed = doneReference.consumed
#            , argument
#            }
#
#    {
#    , mutables = doneArg.mutables
#    , consumed = doneArg.consumed
#    , expression = CA.Call pos doneReference.expression doneArg.argument
#    }


doCallCo as Env: State@: Pos: Expression: [Argument]: { mutables as Dict Name Pos, consumed as Dict Name Pos, expression as Expression } =
    env: state@: pos: reference: arguments:

    doneReference =
        consumed & expression =
            doExpression env @state reference

        { mutables = Dict.empty, consumed, expression }


    doneArgs =
        {
        , mutables = doneReference.mutables
        , consumed = doneReference.consumed
        , arguments = []
        }
        >> List.forReversed arguments arg: s:
            { mutables, consumed, arguments } = s
            da = doArgument env @state pos { mutables, consumed, argument = arg }
            { mutables = da.mutables, consumed = da.consumed, arguments = da.argument :: arguments }

    {
    , mutables = doneArgs.mutables
    , consumed = doneArgs.consumed
    , expression = CA.Call pos doneReference.expression doneArgs.arguments
    }




alias MCA = { mutables as Dict Name Pos, consumed as Dict Name Pos, argument as Argument }

doArgument as Env: State@: Pos: MCA: MCA =
    env: state@: pos: mca:

    try mca.argument as

        CA.ArgumentExpression expr:

            consumed & expression =
                doExpression env @state expr

            Dict.each consumed name: p1:
                try Dict.get name mca.consumed as
                    Nothing: None
                    Just p2: errorReferencingConsumedVariable name p1 p2 @state

            {
            , mutables = mca.mutables
            , consumed = Dict.join consumed mca.consumed
            , argument = CA.ArgumentExpression expression
            }

        CA.ArgumentRecycle p1 name attrPath:

            # TODO https://github.com/xarvh/squarepants/projects/1#card-85087726
            x =
              try Dict.get name env.variables as
                Nothing: errorUndefinedVariable p1 name @state
                Just variable:
                    try variable.mode as
                        Mutable Available: None
                        Immutable: errorMutatingAnImmutable name p1 @state
                        Mutable (ConsumedAt p2): errorMutatingAConsumed name p1 p2 @state

            y =
              try Dict.get name mca.mutables as
                Nothing: None
                Just p2: errorMutatingTwice name p1 p2 @state

            { mca with
            , mutables = Dict.insert name p1 mca.mutables
            , consumed = Dict.empty
            }

        CA.ArgumentRecycle _ _ _:
            Debug.todo "TODO: Mutable args should not be able to reference globals!?"


alias ParsAcc =
    {
    , parsToBeSpent as Dict Name Pos
    , parsToBeRecycled as Dict Name Pos
    , localEnv as Env
    }


doParameter as CA.Parameter: ParsAcc: ParsAcc =
    par: acc:

    try par as
        CA.ParameterPattern pa:
            uniques & localEnv =
                addPatternToEnv pa acc.localEnv

            { acc with
            , parsToBeSpent = Dict.join uniques .parsToBeSpent
            , localEnv
            }

        CA.ParameterRecycle pos name:

            var as Variable =
                {
                , definedAt = pos
                , name
                , mode = Mutable Available
                }

            { acc with
            , parsToBeRecycled = Dict.insert name pos .parsToBeRecycled
            , localEnv = { acc.localEnv with variables = Dict.insert name var .variables }
            }


doExpression as Env: State@: Expression: Dict Name Pos & Expression =
    env: state@: expression:

    re =
        Dict.empty & expression

    try expression as
        CA.LiteralText pos l:
            re

        CA.LiteralNumber pos l:
            re

        CA.Variable pos (CA.RefGlobal _):
            re

        CA.Variable pos (CA.RefLocal name):
            try Dict.get name env.variables as
                Nothing:
                    errorUndefinedVariable pos name @state
                    re

                Just variable:
                    try variable.mode as
                        Immutable:
                            re

                        Mutable Available:
                            Dict.singleton name pos & expression

                        Mutable (ConsumedAt consumedPos):
                            errorReferencingConsumedVariable name pos consumedPos @state
                            Dict.singleton name pos & expression


        CA.Constructor pos usr:
            re

        CA.Fn pos pars body:

            { parsToBeSpent, parsToBeRecycled, localEnv } =
                { parsToBeSpent = Dict.empty, parsToBeRecycled = Dict.empty, localEnv = env }
                >> List.for pars doParameter

            spentByBody & bodyExpression =
                doExpression localEnv @state body

            # variables that are not spent by the body need to be explicitly destroyed
            exprWithDestruction =
                bodyExpression >> Dict.for parsToBeSpent name: pos: exp:
                    if Dict.member name spentByBody then
                        exp
                    else
                        CA.DestroyIn name exp

            # TODO: if spentFromParent /= Dict.empty, this fn should be flagged as unique!!!!
            spentFromParent =
                Dict.diff spentByBody parsToBeSpent

            # check recycled
            Dict.each parsToBeRecycled name: pos:
                if Dict.member name spentByBody then
                    errorConsumingRecycledParameter name pos @state
                else
                    None

            spentFromParent & CA.Fn pos pars exprWithDestruction


#        CA.Call pos reference argument:
#
#            { mutables, consumed, expression = expr } =
#                doCall env @state pos reference argument
#
#            consumed & expr


        CA.Call pos reference arguments:

            { mutables, consumed, expression = expr } =
                doCallCo env @state pos reference arguments

            consumed & expr


        CA.If pos { condition, true, false }:

            consumedByCondition & conditionExpression =
                doExpression env @state condition

            newEnv =
                consumeInEnv consumedByCondition env

            consumedByTrue & trueExpression =
                doExpression newEnv @state true

            consumedByFalse & falseExpression =
                doExpression newEnv @state false

            allConsumed =
                consumedByCondition
                >> Dict.join consumedByTrue
                >> Dict.join consumedByFalse

            finalTrueExpression =
                # true should destroy all muts consumed by false
                trueExpression >> Dict.for consumedByFalse name: pos: exp:
                    if Dict.member name consumedByTrue then
                        exp
                    else
                        CA.DestroyIn name exp

            finalFalseExpression =
                # false should destroy all muts consumed by true
                falseExpression >> Dict.for consumedByTrue name: pos: exp:
                    if Dict.member name consumedByFalse then
                        exp
                    else
                        CA.DestroyIn name exp

            finalExpression =
                CA.If pos {
                    , condition = conditionExpression
                    , true = finalTrueExpression
                    , false = finalFalseExpression
                    }

            allConsumed & finalExpression


        CA.Try pos { value, patternsAndExpressions }:

            consumedByValue & valueExpression =
                doExpression env @state value

            newEnv =
                consumeInEnv consumedByValue env

            # Pass 1: collect all consumed
            consumedAndPatternsAndBlocks =
                patternsAndExpressions >> List.map (pattern & block):

                    mutables_should_be_empty & localEnv =
                        addPatternToEnv pattern newEnv

                    consumedByBlock & blockExpression =
                        doExpression localEnv @state block

                    consumedByBlock & pattern & blockExpression

            allConsumed =
                Dict.empty >> List.for consumedAndPatternsAndBlocks (consumed & _ & _):
                    Dict.join consumed

            # Pass 2:
            newPatternsAndBlocks as [CA.Pattern & Expression]=
                consumedAndPatternsAndBlocks >> List.map (consumed & pattern & blockExpression):
                    finalBlock =
                        blockExpression >> Dict.for allConsumed name: pos: exp:
                            if Dict.member name consumed then
                                exp
                            else
                                CA.DestroyIn name exp

                    pattern & finalBlock

            allConsumed & CA.Try pos { value = valueExpression, patternsAndExpressions = newPatternsAndBlocks }


        CA.Record pos maybeExtending attrValueByName:

            consumedByExt =
                try maybeExtending as
                    Just expr:
                        doExpression env @state expr
                        >> Tuple.first

                    Nothing:
                        Dict.empty

            consumedFinal & attrsFinal =
                consumedByExt & Dict.empty
                >> Dict.for attrValueByName name: value: (consumedSoFar & attrs):
                    consumed & expr =
                        doExpression env @state value

                    consumedTwice =
                        Dict.merge (k: v: d: d) (k: a: b: Dict.insert k (a & b)) (k: v: d: d) consumed consumedSoFar Dict.empty

                    Dict.each consumedTwice name: (p1 & p2):
                        errorReferencingConsumedVariable name p1 p2 @state

                    Dict.join consumed consumedSoFar & Dict.insert name expr attrs

            consumedFinal & CA.Record pos maybeExtending attrsFinal


        CA.RecordAccess pos name expr:
            doExpression env @state expr
            >> Tuple.mapSecond (CA.RecordAccess pos name)


        CA.LetIn valueDef e:

            uniques & env1 =
                addPatternToEnv valueDef.pattern env

            consumedByBody & bodyExpression =
                doExpression env1 @state valueDef.body

            localEnv =
                consumeInEnv consumedByBody env1

            consumedByE & eExpression =
                doExpression localEnv @state e

            finalExpression =
                CA.LetIn { valueDef with body = bodyExpression } eExpression
                >> Dict.for uniques name: pos: exp:
                    try Dict.get name consumedByE as
                        Just _: exp
                        Nothing:
                            CA.DestroyIn name exp

            Dict.join consumedByBody consumedByE & finalExpression


doModule as CA.Module: Res CA.Module =
    module:

    state as State @= {
        , errors = Array.fromList []
        }

    env as Env = {
        , variables = Dict.empty
        }

    addDestruction as pa: CA.ValueDef: CA.ValueDef =
        _: def:

        consumed & body =
            Compiler/UniquenessCheck.doExpression env @state def.body

        { def with body }

    newModule =
        { module with
        , valueDefs = Dict.map addDestruction .valueDefs
        }

    errors =
        Array.toList state.errors

    if errors == [] then
        Ok newModule
    else
        Err (Error.Nested errors)
