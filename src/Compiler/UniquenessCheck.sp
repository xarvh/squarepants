


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






doExpression as Env: State: CA.Expression: Set Name & CA.Expression =
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

            "mutable:- body"
                mutables & localEnv =
                    addPatternToEnv param env

                consumed & bodyExpression =
                    doExpression localEnv @state body

                for each mutables not in consumed:
                    flag them for destruction

            "mutable: body"
                mutables & localEnv =
                    addPatternToEnv param env

                consumed & bodyExpression =
                    doExpression localEnv @state body

                for each mutable:
                    ensure mutable not in consumed

            "immutable: body"
                add all variables to the env


        CA.Call pos reference argument:
            do reference

            look ahead call chain???

            ensure mutable argument is both mutable and not consumed


        CA.If pos { condition, true, false }:
            do condition

            consumed = do true + do false
            to be destroyed in true = false - true
            to be destroyed in false = true - false


        CA.Try pos value patternsAndBlocks:
            ...


        CA.Record pos maybeExtending attrValueByName:
            try maybeExtending as
                Nothing:

                Just extending:


        CA.LetIn valueDef e:
            ...


