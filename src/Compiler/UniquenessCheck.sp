

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


errorConsumingAMutableArgument as Text: Pos: State@: None =
    name: pos: state@:

    addError pos @state eenv: [
        , name .. " is passed as mutable, but the function wants to consume it"
        ]


errorConsumedTwice as Text: Pos: Pos: State@: None =
    name: p1: p2: state@:

    addError p1 @state eenv:

        { location, block } =
            Error.posToHuman eenv p2

        [
        , "You can't consume the variable `" .. name .. "` beause it was consumed already here:"
        , block
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


addPatternToEnv as CA.Pattern: Env: Set Name & Env =
    pattern: env:

    mutabilityByName =
        CA.patternMutabilityByName pattern



    blah =
       name: (isMutable & pos):

       mode =
           if isMutable then Mutable Available else Immutable

       Dict.insert name { definedAt = pos, name, mode }


    localEnv =
        { env with variables = Dict.for mutabilityByName blah .variables }

    names =
        Set.empty >> Dict.for mutabilityByName name: (isMutable & pos): set:
            if isMutable then Set.insert name set else set

    names & localEnv



doCall as Env: State@: Pos: CA.Expression: CA.Argument: { mutables as Dict Name Pos, consumed as Dict Name Pos, expression as CA.Expression } =
    env: state@: pos: reference: argument:

    ref =
        try reference as
            CA.Call p ref arg:
                doCall env @state p ref arg

            _:
              consumed & expression =
                  doExpression env @state reference

              { mutables = Dict.empty, consumed, expression }


    try argument as

        CA.ArgumentExpression expr:

            consumed & expression =
                doExpression env @state expr

            Dict.each consumed name: p1:
                try Dict.get name ref.consumed as
                    Nothing: None
                    Just p2: errorConsumedTwice name p1 p2 @state

            {
            , mutables = ref.mutables
            , consumed = Dict.join consumed ref.consumed
            , expression = CA.Call pos ref.expression (CA.ArgumentExpression expression)
            }

        CA.ArgumentMutable p1 { ref = CA.RefBlock name, attrPath = _ }:

            try Dict.get name env.variables as
                Nothing: errorUndefinedVariable p1 name @state
                Just variable:
                    try variable.mode as
                        Mutable Available: None
                        Immutable: errorMutatingAnImmutable name p1 @state
                        Mutable (ConsumedAt p2): errorMutatingAConsumed name p1 p2 @state

            try Dict.get name ref.mutables as
                Nothing: None
                Just p2: errorMutatingTwice name p1 p2 @state

            {
            , mutables = Dict.insert name p1 ref.mutables
            , consumed = Dict.empty
            , expression = CA.Call pos ref.expression argument
            }

        CA.ArgumentMutable _ _:
            Debug.todo "TODO: Mutable args should not be able to reference globals!?"



doExpression as Env: State@: CA.Expression: Dict Name Pos & CA.Expression =
    env: state@: expression:

    re =
        Dict.empty & expression

    try expression as
        CA.LiteralText pos l:
            re

        CA.LiteralNumber pos l:
            re

        CA.Variable pos { ref = CA.RefRoot _, attrPath }:
            re

        CA.Variable pos { ref = CA.RefBlock name, attrPath }:
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

        CA.Lambda pos param valueLambdaMode body:

            mutables & localEnv =
                addPatternToEnv param env

            consumed & bodyExpression =
                doExpression localEnv @state body

            if mutables == Set.empty then
                consumed & bodyExpression
            else
                try valueLambdaMode as
                    LambdaConsuming:
                        wrappedBody =
                            bodyExpression >> Dict.for mutables name: _: exp:
                                if Dict.member name consumed then
                                    exp
                                else
                                    CA.DestroyIn name exp

                        consumed & wrappedBody

                    LambdaNormal:
                        Dict.each mutables name: _:
                            if Dict.member name consumed then
                                errorConsumingAMutableArgument name pos @state
                            else
                                None

                        consumed & bodyExpression


        CA.Call pos reference argument:

            { mutables, consumed, expression = expr } =
                doCall env @state pos reference argument

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


        CA.Try pos value patternsAndBlocks:

            consumedByValue & valueExpression =
                doExpression env @state value

            newEnv =
                consumeInEnv consumedByValue env

            # Pass 1: collect all consumed
            consumedAndPatternsAndBlocks =
                patternsAndBlocks >> List.map (pattern & block):

                    mutables_should_be_empty & localEnv =
                        addPatternToEnv pattern newEnv

                    consumedByBlock & blockExpression =
                        doExpression localEnv @state block

                    consumedByBlock & pattern & blockExpression

            allConsumed =
                Dict.empty >> List.for consumedAndPatternsAndBlocks (consumed & _ & _):
                    Dict.join consumed

            # Pass 2:
            newPatternsAndBlocks as [CA.Pattern & CA.Expression]=
                consumedAndPatternsAndBlocks >> List.map (consumed & pattern & blockExpression):
                    finalBlock =
                        blockExpression >> Dict.for allConsumed name: pos: exp:
                            if Dict.member name consumed then
                                exp
                            else
                                CA.DestroyIn name exp

                    pattern & finalBlock

            allConsumed & CA.Try pos valueExpression newPatternsAndBlocks


        CA.Record pos maybeExtending attrValueByName:

            consumedByExt =
                try maybeExtending as
                    Just { ref = CA.RefBlock extName, attrPath = _ }:
                        todo "sameAsCA.Variable @state"

                    _:
                        Dict.empty

            todo "CA.Record UniquenesCheck"


        CA.LetIn valueDef e:

            mutables & env1 =
                addPatternToEnv valueDef.pattern env

            consumedByBody & bodyExpression =
                doExpression env @state valueDef.body

            localEnv =
                consumeInEnv consumedByBody env1

            consumedByE & eExpression =
                doExpression localEnv @state e

            finalExpression =
                CA.LetIn { valueDef with body = bodyExpression } eExpression
                >> Dict.for mutables name: pos: exp:
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