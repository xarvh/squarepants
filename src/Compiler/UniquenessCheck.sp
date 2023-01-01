
alias Expression = TA.Expression



union MutableAvailability =
    , Available
    , ConsumedAt Pos


union VariableMode =
    , Immutable
    , Mutable MutableAvailability


alias Variable =
    {
    , definedAt as Pos
    , name as Name
    , mode as VariableMode
    , type as TA.Type
    }


alias Env =
    {
    , variables as Dict Name Variable
    , substitutions as Dict TA.UnificationVariableId TA.Type
    }


alias State =
    {
    , errors as Array Error
    }


#
# This is used as output when evaluating various parts of an expression
#
alias UniOut o =
    {
    # These are uniques that get recycled by an expression
    # Functions cannot be returned outside the scope that declares the uniques they require
    , required as Dict Name Pos

    # These are uniques that are spent by an expression
    , spent as Dict Name Pos

    # This is whatever the output of the function needs to be.
    # It's called "resolved" because all the inferred types should have the type substitutions applied.
    , resolved as o
    }


uniOutInit as a: UniOut a =
    a:
    { required = Dict.empty, spent = Dict.empty, resolved = a }




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


errorConsumingRecycledParameters as Pos: Dict Name Pos: State@: None =
    pos: spentThatShouldHaveBeenRecycled: state@:

    addError pos @state eenv:
        [
        , "errorConsumingRecycledParameters"
        , toHuman spentThatShouldHaveBeenRecycled
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


errorFunctionsCannotConsumeParentUniques as Pos: Dict Name Pos: State@: None =
    pos: spentFromParent: state@:

    addError pos @state eenv: [
        , "This function is consuming these variables, but they were declared in its parent scope: " .. (Dict.keys spentFromParent >> Text.join ", ")
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
        , "the variable `" .. name .. "` can't be used in this call because it is being spent here: "
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
    spent: env:

    translate =
        name: variable:
            try Dict.get name spent as
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


doCall as Env: State@: Pos: Expression: [TA.Argument & TA.Type]: UniOut Expression =
    env: state@: pos: reference: arguments:

    doneReference =
        doExpression env @state reference

    doneArgs as UniOut [TA.Argument & TA.Type] =
        {
        , required = doneReference.required
        , spent = doneReference.spent
        , resolved = []
        }
        >> List.forReversed arguments (arg & type): ({ required, spent, resolved }):
            da = doArgument env @state pos { required, spent, resolved = arg }
            {
            , required = da.required
            , spent = da.spent
            , resolved = (da.resolved & type) :: resolved
            }

    {
    , required = doneArgs.required
    , spent = doneArgs.spent
    , resolved = TA.Call pos doneReference.resolved doneArgs.resolved
    }


doArgument as Env: State@: Pos: UniOut TA.Argument: UniOut TA.Argument =
    env: state@: pos: stuff:

    try stuff.resolved as

        TA.ArgumentExpression expr:

            { spent, required, resolved = expression } =
                doExpression env @state expr

            Dict.each spent name: p1:
                try Dict.get name stuff.spent as
                    Nothing: None
                    Just p2: errorReferencingConsumedVariable name p1 p2 @state

            {
            , required = stuff.required
            , spent = Dict.join spent stuff.spent
            , resolved = TA.ArgumentExpression expression
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
              try Dict.get name stuff.required as
                Nothing: None
                Just p2: errorMutatingTwice name p1 p2 @state

            { stuff with
            , required = Dict.insert name p1 stuff.required
            , spent = Dict.empty
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


doExpression as Env: State@: Expression: UniOut Expression =
    env: state@: expression:

    re =
        { required = Dict.empty, spent = Dict.empty, resolved = expression }

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
                            { required = Dict.empty, spent = Dict.singleton name pos, resolved = expression }

                        Mutable (ConsumedAt consumedPos):
                            errorReferencingConsumedVariable name pos consumedPos @state
                            { required = Dict.empty, spent = Dict.singleton name pos, resolved = expression }


        TA.Constructor pos usr:
            re

        TA.Fn pos pars body:
            # TODO: move this in its own function, it's really big

            { parsToBeSpent, parsToBeRecycled, localEnv } =
                { parsToBeSpent = Dict.empty, parsToBeRecycled = Dict.empty, localEnv = env }
                >> List.for pars (doParameter @state)

            doneBody =
                doExpression localEnv @state body

            #
            # Variables that are not spent by the body need to be explicitly destroyed
            #
            exprWithDestruction =
                doneBody.resolved >> Dict.for parsToBeSpent name: pos: exp:
                    if Dict.member name doneBody.spent then
                        exp
                    else
                        TA.DestroyIn name exp

            #
            # Ensure that the function does not spend any arg that should be recycled!
            #
            spentThatShouldHaveBeenRecycled =
                Dict.intersect doneBody.spent parsToBeRecycled

            if spentThatShouldHaveBeenRecycled /= Dict.empty  then
                errorConsumingRecycledParameters pos spentThatShouldHaveBeenRecycled @state
            else
                None

            #
            # Functions cannot spend variables from the parent scope
            #
            spentFromParent =
                Dict.diff doneBody.spent parsToBeSpent

            if spentFromParent /= Dict.empty then
                errorFunctionsCannotConsumeParentUniques pos spentFromParent @state
            else
                None

            #
            # Expressions "require" all variables from the parent scope that they recycle
            #
            recycledFromParent =
                Dict.diff doneBody.required parsToBeRecycled

            { required = recycledFromParent, spent = Dict.empty, resolved = TA.Fn pos pars exprWithDestruction }


        TA.Call pos reference arguments:
            doCall env @state pos reference arguments


        TA.If pos { condition, true, false }:

            doneCondition =
                doExpression env @state condition

            newEnv =
                consumeInEnv doneCondition.spent env

            doneTrue =
                doExpression newEnv @state true

            doneFalse =
                doExpression newEnv @state false

            finalTrueExpression =
                # true should destroy all muts spent by false
                doneTrue.resolved >> Dict.for doneFalse.spent name: pos: exp:
                    if Dict.member name doneTrue.spent then
                        exp
                    else
                        TA.DestroyIn name exp

            finalFalseExpression =
                # false should destroy all muts spent by true
                doneFalse.resolved >> Dict.for doneTrue.spent name: pos: exp:
                    if Dict.member name doneFalse.spent then
                        exp
                    else
                        TA.DestroyIn name exp

            finalExpression =
                TA.If pos
                    {
                    , condition = doneCondition.resolved
                    , true = finalTrueExpression
                    , false = finalFalseExpression
                    }

            {
            , spent = doneCondition.spent >> Dict.join doneTrue.spent >> Dict.join doneFalse.spent
            , required = doneCondition.required >> Dict.join doneTrue.required >> Dict.join doneFalse.required
            , resolved = finalExpression
            }


        TA.Try pos { type, value, patternsAndExpressions }:

            doneValue =
                doExpression env @state value

            newEnv =
                consumeInEnv doneValue.spent env

            # Pass 1: collect all spent
            donePatternsAndBlocks =
                patternsAndExpressions >> List.map (pattern & block):

                    mutables_should_be_empty & localEnv =
                        addPatternToEnv @state pattern newEnv

                    { spent, required, resolved } = doExpression localEnv @state block
                    { spent, required, resolved = pattern & resolved }

            allSpent =
                Dict.empty >> List.for donePatternsAndBlocks d: Dict.join d.spent

            allRequired =
                Dict.empty >> List.for donePatternsAndBlocks d: Dict.join d.required

            # Pass 2:
            newPatternsAndBlocks as [TA.Pattern & Expression]=
                donePatternsAndBlocks >> List.map ({ spent, required, resolved = pattern & blockExpression }):
                    finalBlock =
                        blockExpression >> Dict.for allSpent name: pos: exp:
                            if Dict.member name spent then
                                exp
                            else
                                TA.DestroyIn name exp

                    pattern & finalBlock

            {
            , spent = allSpent
            , required = allRequired
            , resolved = TA.Try pos { type, value = doneValue.resolved, patternsAndExpressions = newPatternsAndBlocks }
            }


        TA.Record pos maybeExtending attrValueByName:

            doneExt as UniOut (Maybe TA.Expression) =
                try maybeExtending as
                    Nothing:
                        uniOutInit Nothing

                    Just extending:
                        { spent, required, resolved } = doExpression env @state extending
                        { spent, required, resolved = Just resolved }

            doneAttrs as UniOut (Dict Name TA.Expression) =
                uniOutInit Dict.empty
                >> Dict.for attrValueByName name: value: doneSoFar:
                    { spent, required, resolved } =
                        doExpression env @state value

                    consumedTwice =
                        Dict.merge (k: v: d: d) (k: a: b: Dict.insert k (a & b)) (k: v: d: d) spent doneSoFar.spent Dict.empty

                    Dict.each consumedTwice name: (p1 & p2):
                        errorReferencingConsumedVariable name p1 p2 @state

                    {
                    , spent = Dict.join spent doneSoFar.spent
                    , required = Dict.join required doneSoFar.required
                    , resolved = Dict.insert name resolved doneSoFar.resolved
                    }

            {
            , spent = Dict.join doneExt.spent doneAttrs.spent
            , required = Dict.join doneExt.required doneAttrs.required
            , resolved = TA.Record pos doneExt.resolved doneAttrs.resolved
            }


        TA.RecordAccess pos name expr:
            { required, spent, resolved } = doExpression env @state expr
            { required, spent, resolved = TA.RecordAccess pos name resolved }


        TA.LetIn valueDef e:

            uniques & env1 =
                addPatternToEnv @state valueDef.pattern env

            doneDefBody =
                doExpression env1 @state valueDef.body

            localEnv =
                consumeInEnv doneDefBody.spent env1

            doneExpression =
                doExpression localEnv @state e

            finalExpression =
                TA.LetIn { valueDef with body = doneDefBody.resolved } doneExpression.resolved
                >> Dict.for uniques name: pos: exp:
                    try Dict.get name doneExpression.spent as
                        Just _: exp
                        Nothing:
                            TA.DestroyIn name exp

            spent =
                doneExpression.spent
                >> Dict.for uniques (name: _: Dict.remove name)
                >> Dict.join doneDefBody.spent

            {
            , spent
            , required = Dict.join doneDefBody.required doneExpression.required
            , resolved = finalExpression
            }


doModule as TA.Module: Res TA.Module =
    module:

    state as State @=
        {
        , errors = Array.fromList []
        }

    env as Env =
        {
        , variables = Dict.empty
        , substitutions = module.substitutions
        }

    do as pa: TA.ValueDef: TA.ValueDef =
        _: def:

        { spent, required, resolved = body } =
            doExpression env @state def.body

        # TODO Should I check that spent and required are empty?

        { def with body }

    newModule =
        { module with
        , valueDefs = Dict.map do .valueDefs
        }

    errors =
        Array.toList state.errors

    if errors == [] then
        Ok newModule
    else
        Err (Error.Nested errors)
