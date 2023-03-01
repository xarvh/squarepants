#
# This module checks that all rules for uniquess typing is respected.
#

union UniqueAvailability =
    , Available
    , ConsumedAt Pos


union VariableMode =
    , Immutable
    , Unique UniqueAvailability


alias Required = Dict Name { fnPos as Pos, usedAt as Pos }


alias Variable =
    {
    , definedAt as Pos
    , name as Name
    , mode as VariableMode
    , type as TA.FullType
    , required as Required
    }


alias Env =
    {
    , variables as Dict Name Variable
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
    #
    # TODO -------> We don't do this here any more, remove/rename it?
    #
    , resolved as o
    }


uniOutInit as fn a: UniOut a =
    fn a:
    {
    , recycled = Dict.empty
    , required = Dict.empty
    , spent = Dict.empty
    , resolved = a
    }


uniOutMap as fn (fn a: b), UniOut a: UniOut b =
    fn f, ({ recycled, required, spent, resolved }):
    {
    , recycled
    , required
    , spent
    , resolved = f resolved
    }



addError as fn Pos, @state, (fn Error.Env: List Text): None =
    fn pos, @state, messageConstructor:

    Array.push @state.errors (Error.Simple pos messageConstructor)


#
# Errors
#



errorReturnExpressionRequiresUniquesDefinedInTheCurrentScope as fn Name, { usedAt as Pos, fnPos as Pos }, @state: None =
    fn name, ({ usedAt, fnPos }), @state:

    log "IGNORING errorReferencingConsumedVariable" name
    None

#    addError fnPos @state fn eenv:
#
#        { location, block } =
#            Error.posToHuman eenv usedAt
#
#        [
#        , "This expression needs to access the unique variable `" .. name .. "` because it uses it here:"
#        , ""
#        , block
#        , ""
#        , "The problem is that returning a function from the expression could allow accessing `" .. name .. "` from outside of where `" .. name .. "` was declared."
#        , "This would be BAD. [TODO link to wiki]"
#        , ""
#        , "---> COMPILER BUG <--- You are receiving this error even if you are not returning a function. Hopefully I'll be able to fix this soon."
#        ]


errorUniqueHasImmType as fn Name, Pos, TA.FullType, @state: None =
    fn name, pos, type, @state:

    addError pos @state fn eenv:
        [
        , "Variable `" .. name .. "` is unique, but its type is:"
        , toHuman type
        ]



errorReferencingConsumedVariable as fn Text, Pos, Pos, @state: None =
    fn name, pos, consumedPos, @state:

    addError pos @state fn eenv:

        { location, block } =
            Error.posToHuman eenv pos

        cons =
            Error.posToHuman eenv consumedPos

        [
        , "You can't reference again the variable `" .. name .. "` because it was used already here:"
        , cons.block
        ]


errorConsumingRecycledParameters as fn Pos, Dict Name Pos, @state: None =
    fn pos, spentThatShouldHaveBeenRecycled, @state:

    addError pos @state fn eenv:
        [
        , "errorConsumingRecycledParameters"
        , toHuman spentThatShouldHaveBeenRecycled
        ]


errorUndefinedVariable as fn Pos, Text, @state: None =
    fn p, name, @state:

    addError p @state fn eenv: [
        , "undefined variable: " .. name
        ]


errorMutatingAnImmutable as fn Text, Pos, @state: None =
    fn name, p, @state:

    addError p @state fn eenv: [
        , name .. " is immutable, but you are trying to mutate it"
        ]


errorFunctionsCannotConsumeParentUniques as fn Pos, Dict Name Pos, @state: None =
    fn functionPos, spentFromParent, @state:

    addError functionPos @state fn eenv:

        zzz =
            fn (name & spentPos):
              { block, location } = Error.posToHuman eenv spentPos
              block .. "\n"

        blocks =
          spentFromParent
          >> Dict.toList
          >> List.sortBy Tuple.second __
          >> List.map zzz __

        [
        , [ "This function is spending the unique variable `" .. (Dict.keys spentFromParent >> Text.join "`, `" __) .. "`" ]
        , [ "" ]
        , blocks
        , [ "However, functions cannot spend uniques that were declared outside their body." ]
        ]
        >> List.concat


errorConsumingRecycledParameter as fn Text, Pos, @state: None =
    fn name, pos, @state:

    addError pos @state fn eenv: [
        , name .. " is passed as recycled, but the function wants to spend it"
        ]


errorMutatingAConsumed as fn Text, Pos, Pos, @state: None =
    fn name, p2, p1, @state:

    addError p1 @state fn eenv:

        { location, block } =
            Error.posToHuman eenv p2

        [
        , "This code spends the unique variable `" .. name .. "`, but `" .. name .. "` is being used again here:"
        , ""
        , block
        , ""
        , "If you want to use a unique more than once, you need to use a function that recycles it."
        , "TODO: link to uniqueness wiki page"
        ]


errorMutatingTwice as fn Text, Pos, Pos, @state: None =
    fn name, p1, p2, @state:

    addError p1 @state fn eenv:

        { location, block } =
            Error.posToHuman eenv p2

        [
        , name .. " is already being mutated here: "
        , block
        , "You can't use the same unique twice in the same function call"
        , "TODO: link to wiki explaining why"
        ]


consumeInEnv as fn Dict Name Pos, Env: Env =
    fn spent, env:

    translate =
        fn name, variable:
            try Dict.get name spent as
                , Nothing: variable
                , Just pos: { variable with mode = Unique << ConsumedAt pos }

    { env with variables = Dict.map translate .variables }


requireInEnv as fn [Name], Required, Env: Env =
    fn varNames, required, env:

    { env with
    , variables = .variables >> List.for __ varNames fn name, a:
         Dict.update name (Maybe.map (fn var: { var with required }) __) a
    }


addPatternToEnv as fn @state, TA.Pattern, Env: [Name] & Dict Name Pos & Env =
    fn @state, pattern, env:

    names =
        TA.patternNames pattern

    insertVariable =
        fn name, ({ pos, type }), z:

        mode = if type.uni == Imm then Immutable else Unique Available

        variable as Variable =
            {
            , definedAt = pos
            , name
            , mode
            , type
            , required = Dict.empty
            }

        Dict.insert name variable z


    localEnv =
        { env with variables = Dict.for .variables names insertVariable }

    uniques =
        names
        >> Dict.filter (fn n, s: s.type.uni /= Imm) __
        >> Dict.map (fn n, s: s.pos) __

    Dict.keys names & uniques & localEnv


doCall as fn Env, @state, Pos, TA.Expression, [TA.Argument]: UniOut TA.Expression =
    fn env, @state, pos, reference, arguments:

    doneReference =
        doExpression env @state reference

    doneArgs as UniOut [TA.Argument] =
        uniOutInit []
        >> List.forReversed __ arguments fn arg, acc:
            acc
            >> uniOutMap (fn _: arg) __
            >> doArgument env @state pos __
            >> uniOutMap (fn resolvedArg: resolvedArg :: acc.resolved) __

    {
    , recycled = Dict.join doneReference.recycled doneArgs.recycled
    #
    # ---> Since we are calling reference, we can discard doneReference.required!!! <---
    #
    , required = doneArgs.required
    , spent = Dict.join doneReference.spent doneArgs.spent
    , resolved = TA.Call pos doneReference.resolved doneArgs.resolved
    }


doArgument as fn Env, @state, Pos, UniOut TA.Argument: UniOut TA.Argument =
    #
    # NOTE! doArgument does not return only the stuff relevant to the given argument, but rather *accumulates* the result
    #
    fn env, @state, pos, doneSoFar:

    # This is so badly named. This is the argument we want to do.
    # TODO rename 'resolved' to `payload'? Or just use something different for this function. Urgh.
    try doneSoFar.resolved as

        , TA.ArgumentExpression fullType expr:

            doneExpression =
                doExpression env @state expr

            Dict.each doneExpression.spent fn name, p1:
                try Dict.get name doneSoFar.spent as
                    , Nothing: None
                    , Just p2: errorReferencingConsumedVariable name p1 p2 @state

            {
            # We don't count `recycled` in the arguments, because by the time we actually execute the call,
            # the argument evaluation is done.
            , recycled = doneSoFar.recycled
            , required = Dict.join doneExpression.required doneSoFar.required
            , spent = Dict.join doneExpression.spent doneSoFar.spent
            , resolved = TA.ArgumentExpression fullType doneExpression.resolved
            }

        , TA.ArgumentRecycle p1 rawType attrPath name:

            # TODO https://github.com/xarvh/squarepants/projects/1#card-85087726
            x =
              try Dict.get name env.variables as
                , Nothing: errorUndefinedVariable p1 name @state
                , Just variable:
                    try variable.mode as
                        , Unique Available: None
                        , Immutable: errorMutatingAnImmutable name p1 @state
                        , Unique (ConsumedAt p2): errorMutatingAConsumed name p1 p2 @state

            y =
              try Dict.get name doneSoFar.recycled as
                , Nothing: None
                , Just p2: errorMutatingTwice name p1 p2 @state

            { doneSoFar with
            , recycled = Dict.insert name p1 doneSoFar.recycled
            , resolved = TA.ArgumentRecycle p1 rawType attrPath name
            }


alias ParsAcc =
    {
    , parsToBeSpent as Dict Name Pos
    , parsToBeRecycled as Dict Name Pos
    , localEnv as Env
    }


doParameter as fn @state, TA.Parameter, ParsAcc: ParsAcc =
    fn @state, par, acc:

    # TODO provide new param with resolved type

    try par as
        , TA.ParameterPattern fullType pa:
            addedVars & uniques & localEnv =
                addPatternToEnv @state pa acc.localEnv

            { acc with
            , parsToBeSpent = Dict.join uniques .parsToBeSpent
            , localEnv
            }

        , TA.ParameterRecycle pos rawType name:

            var as Variable =
                {
                , definedAt = pos
                , name
                , mode = Unique Available
                , type = { uni = Uni, raw = rawType }
                , required = Dict.empty
                }

            { acc with
            , parsToBeRecycled = Dict.insert name pos .parsToBeRecycled
            , localEnv = { acc.localEnv with variables = Dict.insert name var .variables }
            }


doExpression as fn Env, @state, TA.Expression: UniOut TA.Expression =
    fn env, @state, expression:

    re =
        uniOutInit expression

    try expression as
        , TA.LiteralText pos l:
            re

        , TA.LiteralNumber pos l:
            re

        , TA.Variable pos (RefGlobal _):
            re

        , TA.Variable pos (RefLocal name):
            try Dict.get name env.variables as
                , Nothing:
                    errorUndefinedVariable pos name @state
                    re

                , Just variable:
                    try variable.mode as
                        , Immutable:
                            {
                            , recycled = Dict.empty
                            , required = variable.required
                            , spent = Dict.empty
                            , resolved = expression
                            }

                        , Unique Available:
                            {
                            , recycled = Dict.empty
                            , required = variable.required
                            , spent = Dict.ofOne name pos
                            , resolved = expression
                            }

                        , Unique (ConsumedAt consumedPos):
                            errorReferencingConsumedVariable name pos consumedPos @state
                            {
                            , recycled = Dict.empty
                            , required = variable.required
                            , spent = Dict.ofOne name pos
                            , resolved = expression
                            }

        , TA.Constructor pos usr:
            re

        , TA.Fn pos pars body:
            # TODO: move this in its own function, it's really big

            { parsToBeSpent, parsToBeRecycled, localEnv } =
                { parsToBeSpent = Dict.empty, parsToBeRecycled = Dict.empty, localEnv = env }
                >> List.for __ pars (doParameter @state __ __)

            doneBody =
                doExpression localEnv @state body

            #
            # Variables that are not spent by the body need to be explicitly destroyed
            #
            exprWithDestruction =
                doneBody.resolved >> Dict.for __ parsToBeSpent fn name, _, exp:
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

            if spentThatShouldHaveBeenRecycled == Dict.empty and spentFromParent /= Dict.empty then
                errorFunctionsCannotConsumeParentUniques pos spentFromParent @state
            else
                None

            #
            # Expressions "require" all variables from the parent scope that they recycle
            # and any other variable already required by its body
            #
            required as Required =
                doneBody.recycled
                >> Dict.map (fn k, usedAt: { usedAt, fnPos = pos }) __
                >> Dict.join doneBody.required __
                >> Dict.diff __ parsToBeRecycled

            Dict.each (Dict.join parsToBeRecycled parsToBeSpent) fn varName, parPos:
                try Dict.get varName doneBody.required as
                    , Nothing: None
                    , Just r: errorReturnExpressionRequiresUniquesDefinedInTheCurrentScope varName r @state

            {
            , recycled = Dict.diff doneBody.recycled parsToBeRecycled
            , required
            , spent = Dict.empty
            , resolved = TA.Fn pos pars exprWithDestruction
            }


        , TA.Call pos reference arguments:
            doCall env @state pos reference arguments


        , TA.If pos { condition, true, false }:

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
                doneTrue.resolved >> Dict.for __ doneFalse.spent fn name, _, exp:
                    if Dict.member name doneTrue.spent then
                        exp
                    else
                        TA.DestroyIn name exp

            finalFalseExpression =
                # false should destroy all muts spent by true
                doneFalse.resolved >> Dict.for __ doneTrue.spent fn name, _, exp:
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
            , recycled = doneCondition.recycled >> Dict.join doneTrue.recycled __ >> Dict.join doneFalse.recycled __
            , required = doneCondition.required >> Dict.join doneTrue.required __ >> Dict.join doneFalse.required __
            , spent = doneCondition.spent >> Dict.join doneTrue.spent __ >> Dict.join doneFalse.spent __
            , resolved = finalExpression
            }


        , TA.Try pos { value, valueType, patternsAndExpressions }:

            doneValue =
                doExpression env @state value

            newEnv =
                consumeInEnv doneValue.spent env

            # Pass 1: collect all spent
            zzz = fn (pattern & block):

                    addedVars & mutables_should_be_empty & env0 =
                        addPatternToEnv @state pattern newEnv

                    localEnv =
                        requireInEnv addedVars doneValue.required env0

                    doExpression localEnv @state block
                    >> uniOutMap (fn expr: pattern & expr) __

            donePatternsAndBlocks =
                patternsAndExpressions >> List.map zzz __

            allRecycled =
                Dict.empty >> List.for __ donePatternsAndBlocks fn d, a: Dict.join d.recycled a

            allRequired =
                Dict.empty >> List.for __ donePatternsAndBlocks fn d, a: Dict.join d.required a

            allSpent =
                Dict.empty >> List.for __ donePatternsAndBlocks fn d, a: Dict.join d.spent a

            # Pass 2:
            newPatternsAndBlocks as [TA.Pattern & TA.Expression]=
                xxx =
                    fn ({ recycled, required, spent, resolved = pattern & blockExpression }):
                    finalBlock =
                        blockExpression >> Dict.for __ allSpent fn name, _, exp:
                            if Dict.member name spent then
                                exp
                            else
                                TA.DestroyIn name exp

                    pattern & finalBlock

                List.map xxx donePatternsAndBlocks

            {
            , recycled = allRecycled
            , required = allRequired
            , spent = allSpent
            , resolved = TA.Try pos { valueType, value = doneValue.resolved, patternsAndExpressions = newPatternsAndBlocks }
            }


        , TA.Record pos maybeExtending attrValueByName:

            doneExt as UniOut (Maybe TA.Expression) =
                try maybeExtending as
                    , Nothing:
                        uniOutInit Nothing

                    , Just extending:
                        doExpression env @state extending
                        >> uniOutMap Just __

            doneAttrs as UniOut (Dict Name TA.Expression) =
                uniOutInit Dict.empty
                >> Dict.for __ attrValueByName fn name, value, doneSoFar:
                    { recycled, required, spent, resolved } =
                        doExpression env @state value

                    consumedTwice =
                        Dict.merge (fn k, v, d: d) (fn k, a, b, d: Dict.insert k (a & b) d) (fn k, v, d: d) spent doneSoFar.spent Dict.empty

                    Dict.each consumedTwice fn n, (p1 & p2):
                        errorReferencingConsumedVariable n p1 p2 @state

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


        , TA.RecordAccess pos name expr:
            doExpression env @state expr
            >> uniOutMap (TA.RecordAccess pos name __) __


        , TA.LetIn valueDef e:

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
                >> consumeInEnv doneDefBody.spent __
                >> requireInEnv addedVars doneDefBody.required __

            doneExpression =
                doExpression localEnv @state e

            List.each addedVars fn varName:
                try Dict.get varName doneExpression.required as
                    , Nothing: None
                    , Just r: errorReturnExpressionRequiresUniquesDefinedInTheCurrentScope varName r @state

            finalExpression =
                TA.LetIn { valueDef with body = doneDefBody.resolved } doneExpression.resolved
                >> Dict.for __ uniques fn name, pos, exp:
                    try Dict.get name doneExpression.spent as
                        , Just _: exp
                        , Nothing:
                            TA.DestroyIn name exp

            spent =
                doneExpression.spent
                >> Dict.for __ uniques (fn name, _, d: Dict.remove name d)
                >> Dict.join doneDefBody.spent __

            {
            , recycled = Dict.join doneDefBody.recycled doneExpression.recycled
            #
            # doneDefBody.required will enter this only indirectly, from the expression referencing any addedVars
            #
            , required = doneExpression.required
            , spent
            , resolved = finalExpression
            }


doModule as fn TA.Module: Res TA.Module =
    fn module:

    Debug.benchStart None

    !state as State =
        {
        , errors = Array.fromList []
        }

    env as Env =
        {
        , variables = Dict.empty
        }

    do as fn pa, TA.ValueDef: TA.ValueDef =
        fn _, def:

        doneExpression =
            doExpression env @state def.body

        # TODO Should I check that spent, recycled and required are empty?

        { def with body = doneExpression.resolved }

    newModule =
        { module with
        , valueDefs = Dict.map do .valueDefs
        }

    errors =
        Array.toList @state.errors

    Debug.benchStop "uniqueness check"

    if errors == [] then
        Ok newModule
    else
        Err (Error.Nested errors)

