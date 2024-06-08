#
# This module checks that all rules for uniquess typing is respected.
#

var UniqueAvailability =
    , 'available
    , 'consumedAt Pos


var VariableMode =
    , 'immutable
    , 'unique UniqueAvailability


Required =
    Dict Name { fnPos as Pos, usedAt as Pos }


Variable =
    {
    , definedAt as Pos
    , mode as VariableMode
    , name as Name
    , required as Required
    , type as TA.FullType
    }


Env =
    {
    , modulesByUmr as Dict UMR CA.Module
    , rootDef as TA.RootDef
    , usr as USR
    , variables as Dict Name Variable
    }


State =
    {
    , errors as Array Error
    , lambdas as Hash Int TA.Lambda
    }


getErrorModule as fn Env: Error.Module =
    fn env:
    'USR umr _ =
        env.usr

    try Dict.get umr env.modulesByUmr as
        'nothing: { content = "", fsPath = "N/A" }
        'just { with  asText = content, fsPath }: { content, fsPath }


#
# This is used as output when evaluating various parts of an expression
#
UniOut o =
    {
    # These are uniques that get recycled by an expression
    , recycled as Dict Name Pos
    # If a function recycles uniques outside its scope, we say it "requires" those unique.
    # A function that requires some uniques cannot be returned outside the scope where those uniques are declared.
    , required as Required
    # This is whatever the output of the function needs to be.
    # It's called "resolved" because all the inferred types should have the type substitutions applied.
    #
    # TODO -------> We don't do this here any more, remove/rename it?
    #
    , resolved as o
    # These are uniques that are spent by an expression
    , spent as Dict Name Pos
    }


uniOutInit as fn a: UniOut a =
    fn a:
    {
    , recycled = Dict.empty
    , required = Dict.empty
    , resolved = a
    , spent = Dict.empty
    }


uniOutMap as fn fn a: b, UniOut a: UniOut b =
    fn f, { recycled, required, resolved, spent }:
    {
    , recycled
    , required
    , resolved = f resolved
    , spent
    }


addError as fn Env, Pos, @State, [ Text ]: None =
    fn env, pos, @state, messageConstructor:
    Array.push @state.errors (Error.'simple (getErrorModule env) pos messageConstructor)


#
# Errors
#

errorTaintedCallRecyclesFunctions as fn Env, Pos, Name, Dict Name { fnPos as Pos, usedAt as Pos }, @State: None =
    fn env, callPos, name, required, @state:
    #addFunctions as fn @Array (fn Int: Int): None =
    #      fn @functions:
    #
    #      !x =
    #          1
    #
    #      f as fn Int: Int =
    #          fn n:
    #          @x += 1
    #          n
    #
    #      array_push f @functions
    #      None

    addError
        env
        callPos
        @state
        [
        , "This function call could allow some unique values (" .. (required >> Dict.keys >> Text.join ", " __) .. ")"
        , "to be recycled by a functions contained in the argument `" .. name .. "` outside of the scope where they were declared."
        , "This would be BAD. [TODO link to wiki]"
        , "TODO improve this explanation."
        ]


errorReturnExpressionRequiresUniquesDefinedInTheCurrentScope as fn Env, Name, { fnPos as Pos, usedAt as Pos }, @State: None =
    fn env, name, { fnPos, usedAt }, @state:
    { block, location } =
        Error.posToHuman (getErrorModule env) usedAt

    addError
        env
        fnPos
        @state
        [
        , "This expression needs to access the unique variable `" .. name .. "` because it uses it here:"
        , ""
        , block
        , ""
        , "The problem is that returning a function from the expression could allow accessing `" .. name .. "` from outside of where `" .. name .. "` was declared."
        , "This would be BAD. [TODO link to wiki]"
        ]


errorUniqueHasImmType as fn Env, Name, Pos, TA.FullType, @State: None =
    fn env, name, pos, type, @state:
    addError
        env
        pos
        @state
        [
        , "Variable `" .. name .. "` is unique, but its type is:"
        , toHuman type
        ]


errorReferencingConsumedVariable as fn Env, Text, Pos, Pos, @State: None =
    fn env, name, pos, consumedPos, @state:
    { block, location } =
        Error.posToHuman (getErrorModule env) pos

    cons =
        Error.posToHuman (getErrorModule env) consumedPos

    addError
        env
        pos
        @state
        [
        , "You can't reference again the variable `" .. name .. "` because it was used already here:"
        , cons.block
        ]


errorConsumingRecycledParameters as fn Env, Pos, Dict Name Pos, @State: None =
    fn env, pos, spentThatShouldHaveBeenRecycled, @state:
    addError
        env
        pos
        @state
        [
        , "errorConsumingRecycledParameters"
        , toHuman spentThatShouldHaveBeenRecycled
        ]


errorUndefinedVariable as fn Env, Pos, Text, @State: None =
    fn env, p, name, @state:
    addError
        env
        p
        @state
        [
        , "undefined variable: " .. name
        ]


errorMutatingAnImmutable as fn Env, Text, Pos, @State: None =
    fn env, name, p, @state:
    addError
        env
        p
        @state
        [
        , name .. " is immutable, but you are trying to mutate it"
        ]


errorFunctionsCannotConsumeParentUniques as fn Env, Pos, Dict Name Pos, @State: None =
    fn env, functionPos, spentFromParent, @state:
    zzz =
        fn name & spentPos:
            { block, location } =
                Error.posToHuman (getErrorModule env) spentPos

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
    >> addError env functionPos @state __


errorConsumingRecycledParameter as fn Env, Text, Pos, @State: None =
    fn env, name, pos, @state:
    addError
        env
        pos
        @state
        [
        , name .. " is passed as recycled, but the function wants to spend it"
        ]


errorMutatingAConsumed as fn Env, Text, Pos, Pos, @State: None =
    fn env, name, p2, p1, @state:
    { block, location } =
        Error.posToHuman (getErrorModule env) p2

    addError
        env
        p1
        @state
        [
        , "This code spends the unique variable `" .. name .. "`, but `" .. name .. "` is being used again here:"
        , ""
        , block
        , ""
        , "If you want to use a unique more than once, you need to use a function that recycles it."
        , "TODO: link to uniqueness wiki page"
        ]


errorMutatingTwice as fn Env, Text, Pos, Pos, @State: None =
    fn env, name, p1, p2, @state:
    { block, location } =
        Error.posToHuman (getErrorModule env) p2

    addError
        env
        p1
        @state
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
                'nothing: variable
                'just pos: { variable with mode = 'unique << 'consumedAt pos }

    { env with variables = Dict.map translate .variables }


requireInEnv as fn [ Name ], Required, Env: Env =
    fn varNames, required, env:
    { env with
    , variables =
        .variables
        >> List.for __ varNames fn name, a:
            Dict.update name (Maybe.map (fn var: { var with required }) __) a
    }


addPatternToEnv as fn @State, TA.Pattern, Env: [ Name ] & Dict Name Pos & Env =
    fn @state, pattern, env:
    names =
        TA.patternNames pattern

    insertVariable =
        fn name, { pos, type }, z:
        mode =
            if type.uni == 'imm then 'immutable else 'unique 'available

        variable as Variable =
            {
            , definedAt = pos
            , mode
            , name
            , required = Dict.empty
            , type
            }

        Dict.insert name variable z

    localEnv =
        { env with variables = Dict.for .variables names insertVariable }

    uniques =
        names
        >> Dict.filter (fn n, s: s.type.uni /= 'imm) __
        >> Dict.map (fn n, s: s.pos) __

    Dict.keys names & uniques & localEnv


doCall as fn Env, @State, Pos, TA.LambdaSetId, TA.Expression, [ TA.Argument ]: UniOut TA.Expression =
    fn env, @state, pos, lambdaSet, reference, arguments:
    doneReference =
        doExpression env @state reference

    doneArgs as UniOut [ TA.Argument ] =
        uniOutInit []
        >> List.forReversed __ arguments fn arg, acc:
            acc
            >> uniOutMap (fn _: arg) __
            >> doArgument env @state pos __
            >> uniOutMap (fn resolvedArg: resolvedArg :: acc.resolved) __

    asRecyclingFunction as fn TA.Argument: Maybe Name =
        fn arg:
        try arg as

            TA.'argumentRecycle p raw path name:
                if TA.typeAllowsFunctions (fn tyvarId: 'false) raw then
                    'just name
                else
                    'nothing

            TA.'argumentExpression _ _:
                'nothing

    if doneArgs.required /= Dict.empty or doneReference.required /= Dict.empty then
        List.each (List.filterMap asRecyclingFunction arguments) fn name:
            errorTaintedCallRecyclesFunctions env pos name (Dict.join doneArgs.required doneReference.required) @state
    else
        'none

    {
    , recycled = Dict.join doneReference.recycled doneArgs.recycled
    #
    # ---> Since we are calling reference, we can discard doneReference.required!!! <---
    #
    , required =
        doneArgs.required
    , resolved = TA.'call pos lambdaSet doneReference.resolved doneArgs.resolved
    , spent = Dict.join doneReference.spent doneArgs.spent
    }


doArgument as fn Env, @State, Pos, UniOut TA.Argument: UniOut TA.Argument =
    #
    # NOTE! doArgument does not return only the stuff relevant to the given argument, but rather *accumulates* the result
    #
    fn env, @state, pos, doneSoFar:
    # This is so badly named. This is the argument we want to do.
    # TODO rename 'resolved' to `payload'? Or just use something different for this function. Urgh.
    try doneSoFar.resolved as

        TA.'argumentExpression fullType expr:
            doneExpression =
                doExpression env @state expr

            Dict.each doneExpression.spent fn name, p1:
                try Dict.get name doneSoFar.spent as
                    'nothing: 'none
                    'just p2: errorReferencingConsumedVariable env name p1 p2 @state

            {
            # We don't count `recycled` in the arguments, because by the time we actually execute the call,
            # the argument evaluation is done.
            , recycled =
                doneSoFar.recycled
            , required = Dict.join doneExpression.required doneSoFar.required
            , resolved = TA.'argumentExpression fullType doneExpression.resolved
            , spent = Dict.join doneExpression.spent doneSoFar.spent
            }

        TA.'argumentRecycle p1 rawType attrPath name:
            # TODO https://github.com/xarvh/squarepants/projects/1#card-85087726
            x =
                try Dict.get name env.variables as

                    'nothing:
                        errorUndefinedVariable env p1 name @state

                    'just variable:
                        try variable.mode as
                            'unique 'available: 'none
                            'immutable: errorMutatingAnImmutable env name p1 @state
                            'unique ('consumedAt p2): errorMutatingAConsumed env name p1 p2 @state

            y =
                try Dict.get name doneSoFar.recycled as
                    'nothing: 'none
                    'just p2: errorMutatingTwice env name p1 p2 @state

            { doneSoFar with
            , recycled = Dict.insert name p1 doneSoFar.recycled
            , resolved = TA.'argumentRecycle p1 rawType attrPath name
            }


ParsAcc =
    {
    , localEnv as Env
    , parsToBeRecycled as Dict Name Pos
    , parsToBeSpent as Dict Name Pos
    }


doParameter as fn @State, TA.Parameter, ParsAcc: ParsAcc =
    fn @state, par, acc:
    # TODO provide new param with resolved type

    try par as

        TA.'parameterPattern fullType pa:
            addedVars & uniques & localEnv =
                addPatternToEnv @state pa acc.localEnv

            { acc with
            , localEnv
            , parsToBeSpent = Dict.join uniques .parsToBeSpent
            }

        TA.'parameterPlaceholder fullType n:
            pa =
                TA.'patternAny Pos.'g { maybeName = 'just (Text.fromNumber n), type = fullType }

            addedVars & uniques & localEnv =
                addPatternToEnv @state pa acc.localEnv

            { acc with
            , localEnv
            , parsToBeSpent = Dict.join uniques .parsToBeSpent
            }

        TA.'parameterRecycle pos rawType name:
            var as Variable =
                {
                , definedAt = pos
                , mode = 'unique 'available
                , name
                , required = Dict.empty
                , type = { raw = rawType, uni = 'uni }
                }

            { acc with
            , localEnv = { acc.localEnv with variables = Dict.insert name var .variables }
            , parsToBeRecycled = Dict.insert name pos .parsToBeRecycled
            }


doExpression as fn Env, @State, TA.Expression: UniOut TA.Expression =
    fn env, @state, expression:
    re =
        uniOutInit expression

    try expression as

        TA.'literalText pos l:
            re

        TA.'literalNumber pos l:
            re

        TA.'variable pos ('refGlobal _):
            re

        TA.'variable pos ('refLocal name):
            doVariable env @state pos name expression

        TA.'variable pos ('refPlaceholder n):
            doVariable env @state pos (Text.fromNumber n) expression

        TA.'constructor pos usr:
            re

        TA.'lambda pos lambdaRef context:
            try Dict.get (Tuple.second lambdaRef) env.rootDef.lambdas as
                'nothing: todo "compiler bug: lambda not found"
                'just lambda: doFn env pos @state lambdaRef lambda context

        TA.'call pos lambdaSet reference arguments:
            doCall env @state pos lambdaSet reference arguments

        TA.'if pos { condition, false, true }:
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
                doneTrue.resolved
                >> Dict.for __ doneFalse.spent fn name, _, exp:
                    if Dict.member name doneTrue.spent then
                        exp
                    else
                        TA.'destroyIn name exp

            finalFalseExpression =
                # false should destroy all muts spent by true
                doneFalse.resolved
                >> Dict.for __ doneTrue.spent fn name, _, exp:
                    if Dict.member name doneFalse.spent then
                        exp
                    else
                        TA.'destroyIn name exp

            finalExpression =
                TA.'if
                    pos
                    {
                    , condition = doneCondition.resolved
                    , false = finalFalseExpression
                    , true = finalTrueExpression
                    }

            {
            , recycled = doneCondition.recycled >> Dict.join doneTrue.recycled __ >> Dict.join doneFalse.recycled __
            , required = doneCondition.required >> Dict.join doneTrue.required __ >> Dict.join doneFalse.required __
            , resolved = finalExpression
            , spent = doneCondition.spent >> Dict.join doneTrue.spent __ >> Dict.join doneFalse.spent __
            }

        TA.'try pos { patternsAndExpressions, value, valueType }:
            doneValue =
                doExpression env @state value

            newEnv =
                consumeInEnv doneValue.spent env

            # Pass 1: collect all spent
            zzz =
                fn pattern & block:
                    addedVars & mutables_should_be_empty & env0 =
                        addPatternToEnv @state pattern newEnv

                    localEnv =
                        requireInEnv addedVars doneValue.required env0

                    doExpression localEnv @state block >> uniOutMap (fn expr: pattern & expr) __

            donePatternsAndBlocks =
                patternsAndExpressions >> List.map zzz __

            allRecycled =
                Dict.empty >> List.for __ donePatternsAndBlocks (fn d, a: Dict.join d.recycled a)

            allRequired =
                Dict.empty >> List.for __ donePatternsAndBlocks (fn d, a: Dict.join d.required a)

            allSpent =
                Dict.empty >> List.for __ donePatternsAndBlocks (fn d, a: Dict.join d.spent a)

            # Pass 2:
            newPatternsAndBlocks as [ TA.Pattern & TA.Expression ] =
                xxx =
                    fn { recycled, required, resolved = pattern & blockExpression, spent }:
                    finalBlock =
                        blockExpression
                        >> Dict.for __ allSpent fn name, _, exp:
                            if Dict.member name spent then
                                exp
                            else
                                TA.'destroyIn name exp

                    pattern & finalBlock

                List.map xxx donePatternsAndBlocks

            {
            , recycled = allRecycled
            , required = allRequired
            , resolved = TA.'try pos { patternsAndExpressions = newPatternsAndBlocks, value = doneValue.resolved, valueType }
            , spent = allSpent
            }

        TA.'record pos maybeExtending attrValueByName:
            doneExt as UniOut (Maybe TA.Expression) =
                try maybeExtending as
                    'nothing: uniOutInit 'nothing
                    'just extending: doExpression env @state extending >> uniOutMap 'just __

            doneAttrs as UniOut (Dict Name TA.Expression) =
                uniOutInit Dict.empty
                >> Dict.for __ attrValueByName fn name, value, doneSoFar:
                    { recycled, required, resolved, spent } =
                        doExpression env @state value

                    consumedTwice =
                        Dict.merge (fn k, v, d: d) (fn k, a, b, d: Dict.insert k (a & b) d) (fn k, v, d: d) spent doneSoFar.spent Dict.empty

                    Dict.each consumedTwice fn n, p1 & p2:
                        errorReferencingConsumedVariable env n p1 p2 @state

                    {
                    , recycled = Dict.join recycled doneSoFar.recycled
                    , required = Dict.join required doneSoFar.required
                    , resolved = Dict.insert name resolved doneSoFar.resolved
                    , spent = Dict.join spent doneSoFar.spent
                    }

            {
            , recycled = Dict.join doneExt.recycled doneAttrs.recycled
            , required = Dict.join doneExt.required doneAttrs.required
            , resolved = TA.'record pos doneExt.resolved doneAttrs.resolved
            , spent = Dict.join doneExt.spent doneAttrs.spent
            }

        TA.'recordAccess pos name expr:
            doExpression env @state expr >> uniOutMap (TA.'recordAccess pos name __) __

        TA.'letIn localDef rest restType:
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
                addPatternToEnv @state localDef.pattern env

            doneDefBody =
                doExpression env1 @state localDef.body

            localEnv =
                env1
                >> consumeInEnv doneDefBody.spent __
                >> requireInEnv addedVars doneDefBody.required __

            doneExpression =
                doExpression localEnv @state rest

            if TA.typeAllowsFunctions (fn tyvarId: 'false) restType.raw then
                List.each addedVars fn varName:
                    try Dict.get varName doneExpression.required as
                        'nothing: 'none
                        'just r: errorReturnExpressionRequiresUniquesDefinedInTheCurrentScope env varName r @state
            else
                'none

            finalExpression =
                TA.'letIn { localDef with body = doneDefBody.resolved } doneExpression.resolved restType
                >> Dict.for __ uniques fn name, pos, exp:
                    try Dict.get name doneExpression.spent as
                        'just _: exp
                        'nothing: TA.'destroyIn name exp

            spent =
                doneExpression.spent
                >> Dict.for __ uniques (fn name, _, d: Dict.remove name d)
                >> Dict.join doneDefBody.spent __

            {
            , recycled = Dict.join doneDefBody.recycled doneExpression.recycled
            #
            # doneDefBody.required will enter this only indirectly, from the expression referencing any addedVars
            #
            , required =
                doneExpression.required
            , resolved = finalExpression
            , spent
            }

        TA.'error _:
            re

        TA.'introspect _:
            re


doVariable as fn Env, @State, Pos, Name, e: UniOut e =
    fn env, @state, pos, name, e:
    try Dict.get name env.variables as

        'nothing:
            errorUndefinedVariable env pos name @state

            uniOutInit e

        'just variable:
            try variable.mode as

                'immutable:
                    {
                    , recycled = Dict.empty
                    , required = variable.required
                    , resolved = e
                    , spent = Dict.empty
                    }

                'unique 'available:
                    {
                    , recycled = Dict.empty
                    , required = variable.required
                    , resolved = e
                    , spent = Dict.ofOne name pos
                    }

                'unique ('consumedAt consumedPos):
                    errorReferencingConsumedVariable env name pos consumedPos @state

                    {
                    , recycled = Dict.empty
                    , required = variable.required
                    , resolved = e
                    , spent = Dict.ofOne name pos
                    }


doFn as fn Env, Pos, @State, TA.LambdaRef, TA.Lambda, Dict Name TA.FullType: UniOut TA.Expression =
    fn env, pos, @state, lambdaRef, lambda, context:
    { localEnv, parsToBeRecycled, parsToBeSpent } =
        { localEnv = env, parsToBeRecycled = Dict.empty, parsToBeSpent = Dict.empty } >> List.for __ lambda.pars (doParameter @state __ __)

    doneBody =
        doExpression localEnv @state lambda.body

    #
    # Variables that are not spent by the body need to be explicitly destroyed
    #
    exprWithDestruction =
        doneBody.resolved
        >> Dict.for __ parsToBeSpent fn name, _, exp:
            if Dict.member name doneBody.spent then
                exp
            else
                TA.'destroyIn name exp

    #
    # Ensure that the function does not spend any arg that should be recycled!
    #
    spentThatShouldHaveBeenRecycled =
        Dict.intersect doneBody.spent parsToBeRecycled

    if spentThatShouldHaveBeenRecycled /= Dict.empty then
        errorConsumingRecycledParameters env pos spentThatShouldHaveBeenRecycled @state
    else
        'none

    #
    # Functions cannot spend variables from the parent scope
    #
    spentFromParent =
        Dict.diff doneBody.spent parsToBeSpent

    if spentThatShouldHaveBeenRecycled == Dict.empty and spentFromParent /= Dict.empty then
        errorFunctionsCannotConsumeParentUniques env pos spentFromParent @state
    else
        'none

    #
    # Expressions "require" all variables from the parent scope that they recycle
    # and any other variable already required by its body
    #
    required as Required =
        doneBody.recycled
        >> Dict.map (fn k, usedAt: { fnPos = pos, usedAt }) __
        >> Dict.join doneBody.required __
        >> Dict.diff __ parsToBeRecycled

    if TA.typeAllowsFunctions (fn tyvarId: 'false) lambda.returnType.raw then
        Dict.each (Dict.join parsToBeRecycled parsToBeSpent) fn varName, parPos:
            try Dict.get varName doneBody.required as
                'nothing: 'none
                'just r: errorReturnExpressionRequiresUniquesDefinedInTheCurrentScope env varName r @state
    else
        'none

    Hash.insert @state.lambdas (Tuple.second lambdaRef) { lambda with body = exprWithDestruction }

    {
    , recycled = Dict.diff doneBody.recycled parsToBeRecycled
    , required
    , resolved = TA.'lambda pos lambdaRef context
    , spent = Dict.empty
    }


updateRootDef as fn @Array Error, Dict UMR CA.Module, USR & TA.RootDef: USR & TA.RootDef =
    fn @errors, modulesByUmr, usr & def:
    !state =
        {
        # TODO really need the reassignment op
        , errors =
            cloneUni @errors
        , lambdas = Hash.fromList []
        }

    return =
        try def.body as

            'nothing:
                usr & def

            'just body:
                env as Env =
                    {
                    , modulesByUmr
                    , rootDef = def
                    , usr
                    , variables = Dict.empty
                    }

                doneExpression =
                    doExpression env @state body

                # TODO Should I check that spent, recycled and required are empty?

                lambdas =
                    @state.lambdas
                    >> Hash.toList
                    >> Dict.fromList

                usr & { def with body = 'just doneExpression.resolved }

    @errors := state.errors

    return
