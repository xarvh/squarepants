
alias Expression = TA.Expression



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
    , type as TA.Type
    }


alias Env = {
    , variables as Dict Name Variable
    , substitutions as Dict TA.UnificationVariableId TA.Type
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

errorUniqueHasImmType as Name: Pos: TA.Type: State@: None =
    name: pos: type: state@:

    addError pos @state eenv:
        [
        , "Variable `" .. name .. "` is unique, but its type is:"
        , toHuman type
        ]



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


addPatternToEnv as State@: TA.Pattern: Env: Dict Name Pos & Env =
    state@: pattern: env:

    names =
        TA.patternNames pattern


    insertVariable =
        name: ({ pos, isUnique, type }):

        mode =
           if isUnique then Mutable Available else Immutable

        resolvedType =
            Compiler/TypeCheck.applyAllSubstitutions env.substitutions type

        if isUnique and TA.getUni resolvedType == TA.ForceImm then
            errorUniqueHasImmType name pos resolvedType @state
        else
            None

        Dict.insert name { definedAt = pos, name, mode, type = resolvedType }


    localEnv =
        { env with variables = Dict.for names insertVariable .variables }


    uniques =
        names
        >> Dict.filter (n: s: s.isUnique)
        >> Dict.map (n: s: s.pos)

    uniques & localEnv


doCall as Env: State@: Pos: Expression: [TA.Argument & TA.Type]: { mutables as Dict Name Pos, consumed as Dict Name Pos, expression as Expression } =
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
        >> List.forReversed arguments (arg & type): s:
            { mutables, consumed, arguments } = s
            da = doArgument env @state pos { mutables, consumed, argument = arg }
            { mutables = da.mutables, consumed = da.consumed, arguments = (da.argument & type) :: arguments }

    {
    , mutables = doneArgs.mutables
    , consumed = doneArgs.consumed
    , expression = TA.Call pos doneReference.expression doneArgs.arguments
    }




alias MCA = { mutables as Dict Name Pos, consumed as Dict Name Pos, argument as TA.Argument }

doArgument as Env: State@: Pos: MCA: MCA =
    env: state@: pos: mca:

    try mca.argument as

        TA.ArgumentExpression expr:

            consumed & expression =
                doExpression env @state expr

            Dict.each consumed name: p1:
                try Dict.get name mca.consumed as
                    Nothing: None
                    Just p2: errorReferencingConsumedVariable name p1 p2 @state

            {
            , mutables = mca.mutables
            , consumed = Dict.join consumed mca.consumed
            , argument = TA.ArgumentExpression expression
            }

        TA.ArgumentRecycle p1 attrPath name:

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


alias ParsAcc =
    {
    , parsToBeSpent as Dict Name Pos
    , parsToBeRecycled as Dict Name Pos
    , localEnv as Env
    }


doParameter as State@: TA.Parameter & TA.Type: ParsAcc: ParsAcc =
    state@: (par & type): acc:

    try par as
        TA.ParameterPattern pa:
            uniques & localEnv =
                addPatternToEnv @state pa acc.localEnv

            { acc with
            , parsToBeSpent = Dict.join uniques .parsToBeSpent
            , localEnv
            }

        TA.ParameterRecycle pos name:

            resolvedType =
                Compiler/TypeCheck.applyAllSubstitutions acc.localEnv.substitutions type

            if TA.getUni resolvedType == TA.ForceImm then
                errorUniqueHasImmType name pos resolvedType @state
            else
                None

            var as Variable =
                {
                , definedAt = pos
                , name
                , mode = Mutable Available
                , type = resolvedType
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
        TA.LiteralText pos l:
            re

        TA.LiteralNumber pos l:
            re

        TA.Variable pos (RefGlobal _):
            re

        TA.Variable pos (RefLocal name):
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


        TA.Constructor pos usr:
            re

        TA.Fn pos pars body:

            { parsToBeSpent, parsToBeRecycled, localEnv } =
                { parsToBeSpent = Dict.empty, parsToBeRecycled = Dict.empty, localEnv = env }
                >> List.for pars (doParameter @state)

            spentByBody & bodyExpression =
                doExpression localEnv @state body

            # variables that are not spent by the body need to be explicitly destroyed
            exprWithDestruction =
                bodyExpression >> Dict.for parsToBeSpent name: pos: exp:
                    if Dict.member name spentByBody then
                        exp
                    else
                        TA.DestroyIn name exp

            # TODO: if spentFromParent /= Dict.empty, this fn should be flagged as unique!!!!
            spentFromParent =
                Dict.diff spentByBody parsToBeSpent

            # check recycled
            Dict.each parsToBeRecycled name: pos:
                if Dict.member name spentByBody then
                    errorConsumingRecycledParameter name pos @state
                else
                    None

            spentFromParent & TA.Fn pos pars exprWithDestruction


        TA.Call pos reference arguments:

            { mutables, consumed, expression = expr } =
                doCall env @state pos reference arguments

            consumed & expr


        TA.If pos { condition, true, false }:

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
                        TA.DestroyIn name exp

            finalFalseExpression =
                # false should destroy all muts consumed by true
                falseExpression >> Dict.for consumedByTrue name: pos: exp:
                    if Dict.member name consumedByFalse then
                        exp
                    else
                        TA.DestroyIn name exp

            finalExpression =
                TA.If pos {
                    , condition = conditionExpression
                    , true = finalTrueExpression
                    , false = finalFalseExpression
                    }

            allConsumed & finalExpression


        TA.Try pos { type, value, patternsAndExpressions }:

            consumedByValue & valueExpression =
                doExpression env @state value

            newEnv =
                consumeInEnv consumedByValue env

            # Pass 1: collect all consumed
            consumedAndPatternsAndBlocks =
                patternsAndExpressions >> List.map (pattern & block):

                    mutables_should_be_empty & localEnv =
                        addPatternToEnv @state pattern newEnv

                    consumedByBlock & blockExpression =
                        doExpression localEnv @state block

                    consumedByBlock & pattern & blockExpression

            allConsumed =
                Dict.empty >> List.for consumedAndPatternsAndBlocks (consumed & _ & _):
                    Dict.join consumed

            # Pass 2:
            newPatternsAndBlocks as [TA.Pattern & Expression]=
                consumedAndPatternsAndBlocks >> List.map (consumed & pattern & blockExpression):
                    finalBlock =
                        blockExpression >> Dict.for allConsumed name: pos: exp:
                            if Dict.member name consumed then
                                exp
                            else
                                TA.DestroyIn name exp

                    pattern & finalBlock

            allConsumed & TA.Try pos { type, value = valueExpression, patternsAndExpressions = newPatternsAndBlocks }


        TA.Record pos maybeExtending attrValueByName:

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

            consumedFinal & TA.Record pos maybeExtending attrsFinal


        TA.RecordAccess pos name expr:
            doExpression env @state expr
            >> Tuple.mapSecond (TA.RecordAccess pos name)


        TA.LetIn valueDef e:

            uniques & env1 =
                addPatternToEnv @state valueDef.pattern env

            consumedByBody & bodyExpression =
                doExpression env1 @state valueDef.body

            localEnv =
                consumeInEnv consumedByBody env1

            consumedByE & eExpression =
                doExpression localEnv @state e

            finalExpression =
                TA.LetIn { valueDef with body = bodyExpression } eExpression
                >> Dict.for uniques name: pos: exp:
                    try Dict.get name consumedByE as
                        Just _: exp
                        Nothing:
                            TA.DestroyIn name exp

            Dict.join consumedByBody consumedByE & finalExpression


doModule as TA.Module: Res TA.Module =
    module:

    state as State @= {
        , errors = Array.fromList []
        }

    env as Env = {
        , variables = Dict.empty
        , substitutions = module.substitutions
        }

    addDestruction as pa: TA.ValueDef: TA.ValueDef =
        _: def:

        consumed & body =
            doExpression env @state def.body

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
