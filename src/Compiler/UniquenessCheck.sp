#
# This module checks that all rules for uniquess typing is respected.
#
# In addition, it applies substitutions to all inferred types
#

union MutableAvailability =
    , Available
    , ConsumedAt Pos


union VariableMode =
    , Immutable
    , Mutable MutableAvailability


alias Required = Dict Name { fnPos as Pos, usedAt as Pos }


alias Variable =
    {
    , definedAt as Pos
    , name as Name
    , mode as VariableMode
    , type as TA.Type
    , required as Required
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
    , recycled as Dict Name Pos

    # If a function recycles uniques outside its scope, we say it "requires" those unique.
    # A function that requires some uniques cannot be returned outside the scope where those uniques are declared.
    , required as Required

    # These are uniques that are spent by an expression
    , spent as Dict Name Pos

    # This is whatever the output of the function needs to be.
    # It's called "resolved" because all the inferred types should have the type substitutions applied.
    , resolved as o
    }


uniOutInit as a: UniOut a =
    a:
    {
    , recycled = Dict.empty
    , required = Dict.empty
    , spent = Dict.empty
    , resolved = a
    }


uniOutMap as (a: b): UniOut a: UniOut b =
    f: ({ recycled, required, spent, resolved }):
    {
    , recycled
    , required
    , spent
    , resolved = f resolved
    }



addError as Pos: State@: (Error.Env: List Text): None =
    pos: state@: messageConstructor:

    Array.push @state.errors (Error.Simple pos messageConstructor)


#
# Errors
#



errorReturnExpressionRequiresUniquesDefinedInTheCurrentScope as Name: { usedAt as Pos, fnPos as Pos }: State@: None =
    name: ({ usedAt, fnPos }): state@:

    addError usedAt @state eenv:
        [
        , "Variable `" .. name .. "` is required by a function in the expression... TODO give more details!"
        ]


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


requireInEnv as [Name]: Required: Env: Env =
    varNames: required: env:

    { env with
    , variables = .variables >> List.for varNames name:
         Dict.update name (Maybe.map var: { var with required })
    }


addPatternToEnv as State@: TA.Pattern: Env: [Name] & Dict Name Pos & Env =
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

        variable as Variable =
            {
            , definedAt = pos
            , name
            , mode
            , type = resolvedType
            , required = Dict.empty
            }

        Dict.insert name variable


    localEnv =
        { env with variables = Dict.for names insertVariable .variables }

    uniques =
        names
        >> Dict.filter (n: s: s.isUnique)
        >> Dict.map (n: s: s.pos)

    Dict.keys names & uniques & localEnv


doCall as Env: State@: Pos: TA.Expression: [TA.Argument & TA.Type]: UniOut TA.Expression =
    env: state@: pos: reference: arguments:

    doneReference =
        doExpression env @state reference

    doneArgs as UniOut [TA.Argument & TA.Type] =
        uniOutInit []
        >> List.forReversed arguments (arg & type): acc:
            acc
            >> uniOutMap (_: arg)
            >> doArgument env @state pos
            >> uniOutMap (resolvedArg: (resolvedArg & type) :: acc.resolved)

    {
    , recycled = Dict.join doneReference.recycled doneArgs.recycled
    #
    # ---> Since we are calling reference, we can discard doneReference.required!!! <---
    #
    , required = doneArgs.required
    , spent = Dict.join doneReference.spent doneArgs.spent
    , resolved = TA.Call pos doneReference.resolved doneArgs.resolved
    }


doArgument as Env: State@: Pos: UniOut TA.Argument: UniOut TA.Argument =
    #
    # NOTE! doArgument does not return only the stuff relevant to the given argument, but rather *accumulates* the result
    #
    env: state@: pos: doneSoFar:

    # This is so badly named. This is the argument we want to do.
    # TODO rename 'resolved' to `payload'? Or just use something different for this function. Urgh.
    try doneSoFar.resolved as

        TA.ArgumentExpression expr:

            doneExpression =
                doExpression env @state expr

            Dict.each doneExpression.spent name: p1:
                try Dict.get name doneSoFar.spent as
                    Nothing: None
                    Just p2: errorReferencingConsumedVariable name p1 p2 @state

            {
            , recycled = Dict.join doneExpression.recycled doneSoFar.recycled
            , required = Dict.join doneExpression.required doneSoFar.required
            , spent = Dict.join doneExpression.spent doneSoFar.spent
            , resolved = TA.ArgumentExpression doneExpression.resolved
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
              try Dict.get name doneSoFar.recycled as
                Nothing: None
                Just p2: errorMutatingTwice name p1 p2 @state

            { doneSoFar with
            , recycled = Dict.insert name p1 doneSoFar.recycled
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
            addedVars & uniques & localEnv =
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


doExpression as Env: State@: TA.Expression: UniOut TA.Expression =
    env: state@: expression:

    re =
        uniOutInit expression

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
                            {
                            , recycled = Dict.empty
                            , required = variable.required
                            , spent = Dict.empty
                            , resolved = expression
                            }

                        Mutable Available:
                            {
                            , recycled = Dict.empty
                            , required = variable.required
                            , spent = Dict.singleton name pos
                            , resolved = expression
                            }

                        Mutable (ConsumedAt consumedPos):
                            errorReferencingConsumedVariable name pos consumedPos @state
                            {
                            , recycled = Dict.empty
                            , required = variable.required
                            , spent = Dict.singleton name pos
                            , resolved = expression
                            }

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
            # and any other variable already required by its body
            #
            required as Required =
                doneBody.recycled
                >> Dict.map (k: usedAt: { usedAt, fnPos = pos })
                >> Dict.join doneBody.required
                >> allR: (Dict.diff allR parsToBeRecycled)

            Dict.each (Dict.join parsToBeRecycled parsToBeSpent) varName: parPos:
                try Dict.get varName doneBody.required as
                    Nothing: None
                    Just r: errorReturnExpressionRequiresUniquesDefinedInTheCurrentScope varName r @state

            {
            , recycled = Dict.diff doneBody.recycled parsToBeRecycled
            , required
            , spent = Dict.empty
            , resolved = TA.Fn pos pars exprWithDestruction
            }


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
            , recycled = doneCondition.recycled >> Dict.join doneTrue.recycled >> Dict.join doneFalse.recycled
            , required = doneCondition.required >> Dict.join doneTrue.required >> Dict.join doneFalse.required
            , spent = doneCondition.spent >> Dict.join doneTrue.spent >> Dict.join doneFalse.spent
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

                    addedVars & mutables_should_be_empty & env0 =
                        addPatternToEnv @state pattern newEnv

                    localEnv =
                        requireInEnv addedVars doneValue.required env0

                    doExpression localEnv @state block
                    >> uniOutMap (expr: pattern & expr)

            allRecycled =
                Dict.empty >> List.for donePatternsAndBlocks d: Dict.join d.recycled

            allRequired =
                Dict.empty >> List.for donePatternsAndBlocks d: Dict.join d.required

            allSpent =
                Dict.empty >> List.for donePatternsAndBlocks d: Dict.join d.spent

            # Pass 2:
            newPatternsAndBlocks as [TA.Pattern & TA.Expression]=
                donePatternsAndBlocks >> List.map ({ recycled, required, spent, resolved = pattern & blockExpression }):
                    finalBlock =
                        blockExpression >> Dict.for allSpent name: pos: exp:
                            if Dict.member name spent then
                                exp
                            else
                                TA.DestroyIn name exp

                    pattern & finalBlock

            {
            , recycled = allRecycled
            , required = allRequired
            , spent = allSpent
            , resolved = TA.Try pos { type, value = doneValue.resolved, patternsAndExpressions = newPatternsAndBlocks }
            }


        TA.Record pos maybeExtending attrValueByName:

            doneExt as UniOut (Maybe TA.Expression) =
                try maybeExtending as
                    Nothing:
                        uniOutInit Nothing

                    Just extending:
                        doExpression env @state extending
                        >> uniOutMap Just

            doneAttrs as UniOut (Dict Name TA.Expression) =
                uniOutInit Dict.empty
                >> Dict.for attrValueByName name: value: doneSoFar:
                    { recycled, required, spent, resolved } =
                        doExpression env @state value

                    consumedTwice =
                        Dict.merge (k: v: d: d) (k: a: b: Dict.insert k (a & b)) (k: v: d: d) spent doneSoFar.spent Dict.empty

                    Dict.each consumedTwice name: (p1 & p2):
                        errorReferencingConsumedVariable name p1 p2 @state

                    {
                    , recycled = Dict.join recycled doneSoFar.recycled
                    , required = Dict.join required doneSoFar.required
                    , spent = Dict.join spent doneSoFar.spent
                    , resolved = Dict.insert name resolved doneSoFar.resolved
                    }

            {
            , recycled = Dict.join doneExt.recycled doneAttrs.recycled
            , required = Dict.join doneExt.required doneAttrs.required
            , spent = Dict.join doneExt.spent doneAttrs.spent
            , resolved = TA.Record pos doneExt.resolved doneAttrs.resolved
            }


        TA.RecordAccess pos name expr:
            doExpression env @state expr
            >> uniOutMap (TA.RecordAccess pos name)


        TA.LetIn valueDef e:

            # TODO clean up these notes
            #
            #            !x = 0
            #
            #            f =                <----- f requires x
            #                fn _:
            #                @x += 1
            #
            #            But I can't know what `f` requirements are until I evaluate its body
            #
            #            so `f` will need to be in the env without its requirements properly registered
            #
            #            This is a problem only if f is mutually recursive, but this should not happen because you can't have non-root-level mutual recursion?
            #            only problem: what if 

            addedVars & uniques & env1 =
                addPatternToEnv @state valueDef.pattern env

            doneDefBody =
                doExpression env1 @state valueDef.body

            localEnv =
                env1
                >> consumeInEnv doneDefBody.spent
                >> requireInEnv addedVars doneDefBody.required

            doneExpression =
                doExpression localEnv @state e

            List.each addedVars varName:
                try Dict.get varName doneExpression.required as
                    Nothing: None
                    Just r: errorReturnExpressionRequiresUniquesDefinedInTheCurrentScope varName r @state

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
            , recycled = Dict.join doneDefBody.recycled doneExpression.recycled
            #
            # doneDefBody.required will enter this only indirectly, from the expression referencing any addedVars
            #
            , required = doneExpression.required
            , spent
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

        doneExpression =
            doExpression env @state def.body

        # TODO Should I check that spent, recycled and required are empty?

        { def with body = doneExpression.resolved }

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
