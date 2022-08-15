

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
    , errors as List Error
    }


consumeInEnv as Dict Name Pos: Env: Env =
    consumed: env:

    { env with variables =
        .variables >> Dict.for consumedByValue name: pos:
            Dict.insert name (Mutable << ConsumedAt pos)
    }


addPatternToEnv as CA.Pattern: Env: Set Name & Env =
    pattern: env:

    mutabilityByName =
        CA.patternMutabilityByName pattern

    localEnv =
        { env with variables =
            .variables
            >> Dict.for mutabilityByName name: (isMutable & pos):

                mode =
                    if isMutable then Mutable Available else Immutable

                Dict.insert name { definedAt = pos, name, mode }
        }

    names =
        Set.empty >> Dict.for mutabilityByName name: (isMutable & pos): set:
            if isMutable then Set.insert name set else set

    names & localEnv





doCall as Env: State@: Pos: CA.Expression: CA.Argument: { mutables as Dict Name Pos, consumed as Dict Name Pos, expression as CA.Expression } =
    env: state@: pos: reference: argument:

    ref =
        try reference as
            CA.Call p ref arg:
                doCall env p ref arg

            _:
              consumed & expression =
                  doExpression env @state reference

              { mutables = Dict.empty, consumed, expression }



    try argument as
        CA.ArgumentExpression expr:
            consumed & expression =
                doExpression env @state expr

            TODO check consumed vs ref.consumed

            {
            , mutables = ref.mutables
            , consumed = Dict.join consumed ref.consumed
            , expression = CA.Call pos ref.expression (CA.ArgumentExpression expression)
            }


        CA.ArgumentMutable p { ref = CA.RefBlock name, attrPath = _ }:

            TODO: check that name is
                1) actually mutable
                2) not consumed
                3) not in ref.mutables


            {
            , mutables = Dict.insert name p ref.mutables
            , consumed = Dict.empty
            , expression = CA.Call pos ref.expression argument
            }

        CA.ArgumentMutable _ _:
            Debug.todo "TODO: Mutable args should not be able to reference globals!?"



doExpression as Env: State: CA.Expression: Dict Name Pos & CA.Expression =
    env: state@: expression:

    re =
        Set.empty & expression

    try expression as
        CA.LiteralText pos l:
            re

        CA.LiteralNumber pos l:
            re

        CA.Variable pos { ref = CA.RefRoot _, attrPath }:
            re

        CA.Variable pos { ref = CA.RefName name, attrPath }:
            try Dict.get name env.variables as
                Nothing:
                    addError (errorUndefinedVariable name pos) @state
                    re

                Just variable:
                    try variable.mode as
                        Immutable:
                            re

                        Mutable availability:
                            Set.singleton name & expression

                        Mutable (ConsumedAt consumedPos):
                            addError (errorReferencingConsumedVariable name pos consumedAt) @state
                            Set.singleton name & expression


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
                            bodyExpression >> Set.for mutables name: exp:
                                if Dict.member name consumed then
                                    exp
                                else
                                    CA.DestroyIn name exp

                        consumed & wrappedBody

                    LambdaNormal:
                        Set.for mutables name: tbd:
                            if Set.member name consumed then
                                addError (errorConsumingAMutableArgument name pos) @state
                            else
                                None

                        consumed & bodyExpression


        CA.Call pos reference argument:

            { mutables, consumed, expression } =
                doCall env @state pos reference argument

            consumed & expression



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
            newPatternsAndBlocks =
                consumedAndPatternsAndBlocks >> List.map (consumed & pattern & blockExpression):
                    finalBlock =
                        blockExpression >> Dict.for allConsumed name: pos: exp:
                            if Dict.member name consumed then
                                exp
                            else
                                CA.DestroyIn name exp

            allConsumed & CA.Try pos valueExpression newPatternsAndBlocks


        CA.Record pos maybeExtending attrValueByName:

            consumedByExt =
                try maybeExtending as
                    Just { ref = CA.RefBlock extName, attrPath = _ }:
                        sameAsCA.Variable @state

                    _:
                        Dict.empty


        CA.LetIn valueDef e:
            ...


