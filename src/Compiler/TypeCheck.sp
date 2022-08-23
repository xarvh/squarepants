
[# This module implements what is think is the (Damas)-Hindley-Milner inference algorithm, but I'm not sure.


## Algorithm

The general idea is to have a function that gives me the type of a piece of the AST and checks stuff as it descends into it.
It works on the assumption that the type returned is final, no need to apply substitutions to the whole env.
This means that:

  - We need to order the root definitions we want to infer so that we almost never use a variable before knowing its type
  - We can't infer recursive functions unless they are annotated (this is a limitation I will want to overcome, just not now)

The one time where we don't have the type beforehand is with the argument of lambdas, but even then we try to minimize the
substitutions necessary.

??? Whenever we find a substitution for a tyvar, we apply it at once, because we need to get rid of that tyvar everywhere it appears?


## NonFunction

Squarepants can flag its type variables as "NonFunction", which means they will cause an error if they are replaced with a
type that is or contains a function or a type variable that will accept functions.

I guess it works like some sort of typeclass?

Anyway, every time [we create a substitution / we replace a tyvar?] that has the NonFunction flag, we must check that the
replacing type does not contain functions or tyvars that allow functions (ie, tyvars without the NonFunction flag)

The error should be something like "Cannot replace `a` with `b` because `b` is allowed to contain functions"?


## Mutation

Mutation requires us to enforce the following rules:

1.  A mutable value is always NonFunction
2.  A block that defines a mutable can't return any type that [contains a function / allows a function?]

Mutability enters the algorithm at 3 different points:

  - when we're inferring a call argument and the argument is marked mutable
    We know already the argument type, so we can type check it already.

  - when we're inferring a mutable lambda parameter
    Since we create a tyvar for the parameter, we can set it already as non-function

  - when we're inferring a block that defines mutables
    This is more complicated.

    If the return type contains functions, we reject it outright.

    But what about when it contains type variables?

      - A tyvar can be generated internally by the block:

        1.  If the return value contains a function like `(a: a)`, but functions can't be returned as we said above.

        2.  Functions that never actually return, ie `Debug.crash as Text: a`

        3.  Union constructors that actually don't instantiate the type, such as `[] as List` and `Nothing as Maybe a`,
            but only if the function never returns the populated constructors.

        Case 1. can't happen.

        For case 2. and 3. the type is actually irrelevant, so maybe we can get away with not even converting the tyvar?

      - A tyvar could come from the parent scope.
        Thing is, even if the tyvar contains a function, it still can't be used as a function internally.
        This means that blocks that define mutables _CAN_ return functions as long as they don't know they're functions!!
        Put it another way, a block that defines mutables _can_ return tyvars that allow functions, but must still be
        prevented from returning definite functions.

        This is different than what NonFunction does.

        It also makes thinking about Squarepants' rules a bit harder, since sometimes it's "no functions and no free vars"
        and sometimes it's "no functions but free vars are ok".

#]


alias Type =
    CA.Type


typeToText as Env: Type: Text =
    env:
    HCA.typeToText env.currentModule env.meta



#
# Env
#

# rename to Scope?
alias Env = {
    , currentModule as Meta.UniqueModuleReference
    , meta as Meta
    , instanceVariables as Dict CA.Ref CA.InstanceVariable
    , constructors as CA.All CA.Constructor
    , types as CA.All CA.TypeDef
    [#
       Every time we use a value in an expression, we must re-instantiate its free variables, because each time they can be used in a different way.

       Because of this each Definition has a `freeTypeVariables` field.

       To figure out `freeTypeVariables` we can't simply take all tyvars we find in the definition's type, because:

       1) a tyvar that is free in a parent definition should not be considered free in children definitions:

         parent =
           as List a

           child =
             as a: a
             ...

           ...

       2) the placeholder tyvar of an argument should not be considered free while we infer the type of a function, otherwise we keep reinstantitiating it and we never figure out anything.

         f x =
           { first } = x
           first

       By keeping track of which tyvars should NOT be considered free, we can figure out the correct `freeTypeVariables` field for each child definition.
    #]
    , nonFreeTyvars as Dict Name CA.TyvarFlags

    # This is used to produce nicer errors for when a recursive function is not annotated
    , nonAnnotatedRecursives as Dict Name Pos
    }


#
# State
#


alias State =
    { nextName as Int
    , errors as List Error
    , substitutions as Subs

    # Type variables that cannot accept functions
    # TODO None should be RejectReasons, but am not sure we need it at all
    , nonFnTyvars as Dict Name (List None)

    # This is used only by the unify function, which resets it every time
    #
    # When an unification fails, a new placeholder type variable is returned, and the two clashing types are recorder here
    #
    , typeClashesByPlaceholderId as Maybe (Dict Name TypeClash)
    }


initState as State = {
    , nextName = 0
    , errors = []
    , substitutions = Dict.empty
    , nonFnTyvars = Dict.empty
    , typeClashesByPlaceholderId = Nothing
    }


alias TypeClash =
    { t1 as Type
    , t2 as Type
    , err as UnifyError
    }


alias Subs =
    Dict Name Type



#
# State monad
#
# TODO: replace with mutable
#


alias Monad a =
    StateMonad.M State a


#get =
get as (state: a): StateMonad.M state a =
    StateMonad.get
#    as (State: a): State @: a
#
#    f s


#return =
return as a: StateMonad.M state a =
    StateMonad.return
#    as a: Monad a
#    a


#andThen =
andThen as (a: StateMonad.M state b): StateMonad.M state a: StateMonad.M state b =
    StateMonad.andThen
#    as (a: State @: b): State @: a: State @: b
#
#    f (m @s) @s


list_map2 as (a: b: StateMonad.M state c): List a: List b: StateMonad.M state (List c) =
    StateMonad.list_map2
#list_map2 =
#    as (x: y: State @: z): [x]: [y]: State @: [z]
#
#    List.map2 (fn xx yy: f xx yy @s) xs ys


list_for as List item: (item: accum: StateMonad.M state accum): accum: StateMonad.M state accum =
    l: f:
    StateMonad.list_foldl f l
#list_foldl = #f xs init @s =
#    as (x: acc: State @: acc): [x]: State @: acc
#
#    List.foldl (fn x acc: f x acc @s) xs init


dict_map as (comparable: a: StateMonad.M state b): Dict comparable a: StateMonad.M state (Dict comparable b) =
    StateMonad.dict_map
#dict_map = # f d @s =
#    as (k: v: State @: o): Dict k v: State @: (Dict k o)
#
#    Dict.map (fn k v: f k v @s) d


dict_for as Dict comparable item: (comparable: item: accum: StateMonad.M state accum): accum: StateMonad.M state accum =
    d: f:
    StateMonad.dict_foldl f d
#dict_foldl = # f d z @s =
#    as (k: v: a: State @: a): Dict k v: State @: a
#
#    Dict.foldl (fn k v a: f k v a @s) d z

m_update as (state: state): StateMonad.M state state =
    StateMonad.update





#
#
#



expandAlias as Type: Type =
    type:
    try type as
        CA.TypeAlias _ _ t: expandAlias t
        _: type



newName as (Name: a): Monad a =
    f: state:
    f (Text.fromNumber state.nextName) & { state with nextName = state.nextName + 1 }


newType as Pos: CA.TyvarFlags: Monad Type =
    pos: flags:
    newName (n: CA.TypeVariable pos n flags)


insertError as Error: Monad None =
    e: state:
    None & { state with errors = e :: state.errors }


setNonFn as Name: Monad None =
    name: state:
    # TODO value should not be an empty list?
    None & { state with nonFnTyvars = Dict.insert name [] state.nonFnTyvars }



insertTypeClash as Name: Type: Type: UnifyError: Monad None =
    id: t1: t2: err: state:
    try state.typeClashesByPlaceholderId as
        Nothing:
            { id = id
            , t1 = t1
            , t2 = t2
            , err = err
            }
                >> toHuman
                >> x: "Inserting type clash outside of unify" .. x
                >> todo

        Just dict:
            x =
                dict
                    >> Dict.insert id { t1 = t1, t2 = t2, err = err }
                    >> Just

            None & { state with typeClashesByPlaceholderId = x }


popClashingtypes as Monad (Dict Name TypeClash) =
    state:
    try state.typeClashesByPlaceholderId as
        Nothing:
            todo "popping a nothing!"

        Just dict:
            dict & { state with typeClashesByPlaceholderId = Nothing }


getFreeTypeVars as Dict Name a: Type: Dict Name CA.TyvarFlags =
    nonFreeTyvars: ty:

    typeTyvars ty
    >> Dict.filter (name: flags: not << Dict.member name nonFreeTyvars)


typeTyvars as Type: Dict Name CA.TyvarFlags =
    ty:
    try ty as
        CA.TypeVariable pos name flags:
            # TODO is pos equivalent to definedAt?
            Dict.singleton name flags

        CA.TypeFunction _ from fromIsMutable to:
            Dict.join (typeTyvars from) (typeTyvars to)

        CA.TypeConstant pos ref args:
            List.for args (a: Dict.join (typeTyvars a)) Dict.empty

        CA.TypeAlias _ path t:
            typeTyvars t

        CA.TypeMutable _ t:
            typeTyvars t

        CA.TypeRecord pos attrs:
            Dict.empty
            >> Dict.for attrs (n: t: Dict.join (typeTyvars t))

        CA.TypeRecordExt pos name flags attrs:
            Dict.singleton name flags
            >> Dict.for attrs (n: t: Dict.join (typeTyvars t))

#
# Inference
#


fromModule as Env: CA.Module: Res Env =
    env: module:

    Debug.benchStart None

    insert as CA.Pattern: CA.ValueDef: [CA.ValueDef] & [CA.ValueDef]: [CA.ValueDef] & [CA.ValueDef] =
        pa: def: (ann & nonAnn):
        allAnnotated =
            pa
                >> CA.patternNamedTypes
                >> Dict.values
                >> List.all (pos & maybeType): maybeType /= Nothing

        if allAnnotated then
            (def :: ann) & nonAnn
        else
            ann & (def :: nonAnn)

    annotated & nonAnnotated =
        Dict.for module.valueDefs insert ([] & [])

    try nonAnnotated as
        first :: second :: tail:
            [# TODO

                try RefHierarchy.reorder (x: x.name) getValueRefs nonAnnotated as
                    Err circulars:
                        # TODO test this error. Is it "circular" or "recursive"?
                        "These definitions are recursive but don't have a type annotation: " .. Text.join ", " (circulars >> Set.fromList >> Set.toList)
                            >> Error.errorTodo

                    Ok orderedNonAnnotated:

                        allOrdered =
                            orderedNonAnnotated .. annotated

                        ( envF & stateF ) =
                            annotated
                                >> List.for env insertAnnotatedRootValue
                                >> M.list_for allOrdered fromRootDefinition
                                >> M.run initState

            #]
            pos =
                CA.patternPos first.pattern

            names =
                nonAnnotated
                    >> List.concatMap (d: d.pattern >> CA.patternNamedTypes >> Dict.keys)

            Error.res (CA.patternPos first.pattern) eenv: [
                , "Support for non-annotated root definitions is not yet implemented. =*("
                , "These definitions need an annotation: " .. Text.join ", " names
                ]
                    >> btw Debug.benchStop "type check"

        _:
            # Don't need to reorder a single non-annotated! =D
            # (Most tests define only one, this is why I'm doing this...)
            # TODO Just reorder them already -_-
            orderedNonAnnotated =
                nonAnnotated

            allOrdered =
                List.concat [ orderedNonAnnotated, annotated ]

            envF & stateF =
                env
                    >> list_for allOrdered (fromDefinition True)
                    >> StateMonad.run initState

            if stateF.errors == [] then
                Ok envF
                    >> btw Debug.benchStop "type check"

            else
                stateF.errors
                    >> Error.Nested
                    >> Err
                    >> btw Debug.benchStop "type check"


[# TODO It might be faster if we write down the references while we build the canonical AST
TODO It might be _even faster_ if we use the references to decide what to compile and what not to.
#]
#getValueRefs def =
#    as CA.RootValueDef: Set Text
#
#    fun fold ext =
#        as CA.PosMap: a: M (Set Text) a
#        try fold as
#            CA.PosMap_Expr (CA.Variable _ args):
#                if args.isRoot then
#                    (M.update (Set.insert args.name)) >> andThen _:
#                    return ext
#
#                else
#                    return ext
#
#            _:
#                return ext
#
#    Set.empty
#        >> CA.posMap_rootValueDef fun def
#        >> Tuple.second



applySubsToType as Type: Monad Type =
    ty:
    get (x: x.substitutions) >> andThen subs:
    return << replaceTypeVariables subs ty


fromDefinition as Bool: CA.ValueDef: Env: Monad Env =
    isRoot: def: env:

    fromPattern env def.pattern Dict.empty >> andThen patternOut:

    ip =
        insertPatternVars
            { subs = Dict.empty
            , isParameter = False
            , isRoot
            }
            patternOut.vars
            env

    ip >> andThen env1:

    if def.native then
        return env1

    else if patternOut.isFullyAnnotated then
        #TODO!!! replacing patternOut.ty with patternOut.type gives a super useless error
        checkExpression env1 patternOut.ty def.body >> andThen None:
        return env1

    else
        fromExpression env1 def.body >> andThen bodyType_:
        applySubsToType bodyType_ >> andThen bodyType:
        unify env1 patternOut.pos UnifyReason_DefBlockVsPattern bodyType patternOut.ty >> andThen unifiedType:

        checkFreeVariables env1 patternOut.pos patternOut.ty bodyType >> andThen None:

        # We need to apply the subs to the tyvars that were assigned to any non-annotated name.
        #
        # TODO this can probably be cleaned up, I don't like to be calling insertPatternVars twice
        # so I should probably review how the function is used in the other two places and figure out
        # a cleaner way to do it
        applySubsToNonFreeTyvars env1 >> andThen env2:
        get (x: x.substitutions) >> andThen subs:
        insertPatternVars
            { subs
            , isParameter = False
            , isRoot
            }
            patternOut.vars
            env2


checkFreeVariables as Env: Pos: Type: Type: Monad None =
    env: pos: patternType: blockType:

    annotatedFreeVars =
        typeTyvars patternType
            >> Dict.filter (name: _: isAnnotation name)

    actualFreeVars =
        typeTyvars blockType

    if Dict.size annotatedFreeVars > Dict.size actualFreeVars then
        [ "The annotation is too general"
        , ""
        , "The annotation uses: " .. Text.join ", " (Dict.keys annotatedFreeVars)
        #TODO, typeToText annotatedType
        , ""
        , "But the actual type uses only: " .. Text.join ", " (Dict.keys actualFreeVars)
        , typeToText env blockType
        , ""
        , "The annotation has " .. Text.fromNumber (Dict.size annotatedFreeVars - Dict.size actualFreeVars) .. " type variables too many"
        ]
            >> addError pos
            >> andThen _:
                return None

    else
        return None



########


isCompatibleWith as Env: Type: Pos: Type: Monad None =
    env: expectedType: pos: actualType:

    try expectedType & actualType as

      CA.TypeAlias _ _ ty & _:
          isCompatibleWith env ty pos actualType

      _ & CA.TypeAlias _ _ ty:
          isCompatibleWith env expectedType pos ty

      CA.TypeConstant _ expectedUsr expectedArgs & CA.TypeConstant _ actualUsr actualArgs:
          if expectedUsr /= actualUsr then
              addCheckError pos [
                  , "This expression should be of type"
                  , typeToText env expectedType
                  , "but instead is"
                  , typeToText env actualType
                  ]
          else
            # I don't need to check that arg length is the same, do I?
            None >> list_for (List.map2 Tuple.pair expectedArgs actualArgs) (e & a): None:
                isCompatibleWith env e pos a


      CA.TypeFunction _ eFrom eIsMut eTo & CA.TypeFunction _ aFrom aIsMut aTo:
          if eIsMut /= aIsMut then
              addCheckError pos [
                  , "mutability clash"
                  ]
          else
              isCompatibleWith env eFrom pos aFrom >> andThen _:
              isCompatibleWith env eTo pos aTo

      CA.TypeRecord _ eAttrs & _:
          recordIsCompatibleWith env pos eAttrs actualType

      CA.TypeRecordExt _ e flags eAttrs & _:
          todo "Extensible record annotation is experimentally disabled"

      CA.TypeMutable _ t1 & CA.TypeMutable _ t2:
          isCompatibleWith env t1 pos t2

      _ & CA.TypeVariable _ actualName flags:
          unify env pos UnifyReason_AnnotationSimple expectedType actualType >> andThen unifiedArgumentType:


          return None
#          try Dict.get actualName env.nonFreeTyvars as
#              Nothing:
#
#              Just nonFreeTyvar:
#                  addCheckError pos [
#                      , "Different type variables"
#                      , "Annotation uses `" .. typeToText env expectedType .. "`"
#                      , "But actual type is `" .. actualName .. "` which is non-free: " .. toHuman nonFreeTyvar
#                      ]

      _:
          addCheckError pos [
            , "I was expecting"
            , typeToText env expectedType
            , "but the actual type is: "
            , typeToText env actualType
            , "The two types are not compatible!"
            ]


addCheckConstructorError as Pos: Env: [CA.Pattern]: [Text]: Monad Env =
    pos: env: remainingArgs: message:

    addCheckError pos message >> andThen None:

    env >> list_for remainingArgs argPattern: envX:
        return envX
        [# TODO add remaining args to env
          fromPattern envX argPattern Dict.empty >> andThen patternOut:

          { env with instanceVariables =
              .instanceVariables
                  >> Dict.insert (CA.RefBlock name) {
                      , definedAt = pos
                      , ty = annotation
                      , freeTypeVariables = CA.getFreeTypeVars env.nonFreeTyvars Dict.empty annotation
                      , isMutable = False
                      }
          }

          ...

          insertPatternVars
              { subs = Dict.empty
              , isParameter = False
              , isMutable = False
              , isRoot
              }
              patternOut.vars
              env
        #]



checkAndInsertAnnotatedPattern as Env: Type: Pos: Bool: Maybe Text: Maybe Type: Monad Env =
    env: expectedType: pos: isMutable: maybeName: maybeAnnotation:

    envWith =
        name: type:
        { env with instanceVariables =
            # TODO why keeping `env` implicit gives a shitton of errors?
            env.instanceVariables
                >> Dict.insert (CA.RefBlock name) {
                    , definedAt = pos
                    , ty = type
                    , freeTypeVariables = getFreeTypeVars env.nonFreeTyvars type
                    , isMutable
                    }
        }

    try maybeName & maybeAnnotation as

        Just name & Just annotation:
            isCompatibleWith env expectedType pos annotation >> andThen None:
            return (envWith name annotation)

        Just name & Nothing:
            return (envWith name expectedType)

        Nothing & Just annotation:
            isCompatibleWith env expectedType pos annotation >> andThen None:
            return env

        Nothing & Nothing:
            return env


checkAndInsertPattern as Env: Type: CA.Pattern: Monad Env =
    env: expectedType_: pattern:

    expectedType =
        expandAlias expectedType_

    try pattern as

        CA.PatternAny pos isMutable maybeName maybeAnnotation:
            checkAndInsertAnnotatedPattern env expectedType pos isMutable maybeName maybeAnnotation

        CA.PatternLiteralNumber pos literal:
            todo "TODO needs proper type comparison without `pos`"
            if expectedType == CoreTypes.number then

                return env
            else
                addCheckError pos [ "This pattern is a Number, but the annotation says it should be " .. typeToText env expectedType ] >> andThen None:
                return env

        CA.PatternLiteralText pos literal:
            todo "TODO needs proper type comparison without `pos`"
            if expectedType == CoreTypes.text then
                return env
            else
                addCheckError pos [ "This pattern is a Text, but the annotation says it should be " .. typeToText env expectedType ] >> andThen None:
                return env

        CA.PatternConstructor pos usr args:
            try Dict.get usr env.constructors as
                Nothing:
                    addCheckConstructorError pos env args [ "Unknown constructor: " .. toHuman usr ]

                Just constructor:
                    try expectedType as
                        # TODO!!! calling this `args` instead of `args_` should cause a shadowing error, why doesn't it trigger?
                        CA.TypeConstant _ expectedUsr args_:
                            if usr /= constructor.typeUsr then
                                addCheckConstructorError pos env args [
                                    , "Constructor produces " .. toHuman constructor.typeUsr .. " but annotation requires " .. toHuman expectedUsr
                                    ]
                            else
                                # TODO!!! they type args should be applied to constructor.args
                                checkConstructorWithItsArgs env pos 0 args constructor.args

                        _:
                            addCheckConstructorError pos env args [
                                "This pattern is an union type, but the annotation expects a " .. typeToText env expectedType
                            ]

        CA.PatternRecord pos patternAttrs:
            zzzz =
                expectedTypeAttrs:
                env >> dict_for patternAttrs attrName: attrPattern: envX:
                    try Dict.get attrName expectedTypeAttrs as
                        Nothing:
                            addCheckError pos [ "This record pattern has an attribute `" .. attrName .. "` but it is not avaiable in the annotation" ] >> andThen None:
                            return envX
                        Just expectedAttrType:
                            checkAndInsertPattern envX expectedAttrType attrPattern

            try expectedType as

                CA.TypeRecord _ expectedTypeAttrs:
                    zzzz expectedTypeAttrs

                CA.TypeRecordExt _ _ _ expectedTypeAttrs:
                    zzzz expectedTypeAttrs

                _:
                    addCheckError pos [ "This pattern is a record, but the annotation says it should be " .. typeToText env expectedType ] >> andThen None:
                    return env


checkConstructorWithItsArgs as Env: Pos: Int: [CA.Pattern]: [CA.Type]: Monad Env =
    env: pos: index: actualArgs: expectedArgs:
    try actualArgs & expectedArgs as

        (actualHead :: actualTail) & (expectedHead :: expectedTail):
            checkAndInsertPattern env expectedHead actualHead >> andThen updatedEnv:
            checkConstructorWithItsArgs updatedEnv pos (index + 1) actualTail expectedTail

        [] & (expectedHead :: expectedTail):
            given = index
            needed = index + List.length expectedArgs
            addCheckError pos [ "Constructor needs " .. Text.fromNumber needed .. " arguments but was given only " .. Text.fromNumber given ] >> andThen None:
            return env

        (actualHead :: actualTail) & []:
            addCheckConstructorError pos env actualArgs [ "more arguments than needed" ]

        [] & []:
            return env









checkExpression as Env: Type: CA.Expression: Monad None =
    env: expectedType_: expression:

    expectedType =
        expandAlias expectedType_

    try expression as
        CA.LiteralText pos l:
            isCompatibleWith env expectedType pos CoreTypes.text

        CA.LiteralNumber pos l:
            isCompatibleWith env expectedType pos CoreTypes.number

        CA.Variable pos { ref, attrPath }:
            try Dict.get ref env.instanceVariables as
                Nothing:
                    # TODO: errorUndefinedVariable should return `Monad None`
                    # Then I should have a helper to add a fresh tyvar when inferring?
                    errorUndefinedVariable env pos ref >> andThen _:
                    return None

                Just var:
                    #
                    # x as [Text] =
                    #     []
                    #
                    # TODO: create an `instantiateVariable` function?
                    # TODO: what if we instantiate /after/ we applyAttributePath? Would it be faster?
                    replaceTypeVariablesWithNew var.freeTypeVariables var.ty >> andThen instantiatedType:
                    applyAttributePath env pos attrPath instantiatedType >> andThen ty:
                    isCompatibleWith env expectedType pos ty

        CA.Constructor pos usr:
            try Dict.get usr env.constructors as
                Nothing:
                    errorUndefinedVariable env pos (CA.RefRoot usr) >> andThen _:
                    return None

                Just c:
                    replaceTypeVariablesWithNew (getFreeTypeVars Dict.empty c.type) c.type >> andThen instantiatedType:
                    #
                    # x as Maybe Text =
                    #     Nothing
                    #
                    isCompatibleWith env expectedType pos instantiatedType

        CA.Lambda pos param valueLambdaMode body:
            try expectedType as
                CA.TypeFunction _ parameterType typeLambdaMode returnType:
                    xxx =
                        if valueLambdaMode /= typeLambdaMode then
                            addCheckError pos [
                                , "the function and the annotation have different mutability"
                                ]
                        else
                            return None

                    xxx >> andThen _:
                    checkAndInsertPattern env parameterType param >> andThen localEnv:
                    checkExpression localEnv returnType body

                CA.TypeFunction _ _ typeLambdaMode _:
                    addCheckError pos [
                        , "the function and the annotation have different mutability"
                        ]

                CA.TypeVariable pos name flags:


                    todo "2309478y3qp"

                    if isAnnotation name then
                        addCheckError pos [
                            , "This is a function, but the annotation says it should be of type variable `" .. name .. "` which implies that it could be of any type!"
                            ]
                    else
                        fromExpression env expression >> andThen actualType:
                        unify env pos UnifyReason_IsLambda expectedType actualType >> andThen _:
                        return None

                _:
                    addCheckError pos [
                        , "This is a function, but the annotation says it should be a: "
                        , typeToText env expectedType
                        ]

        CA.Call pos reference argument:

            fromExpression env reference >> andThen referenceType_:

            referenceType =
                expandAlias referenceType_

            try referenceType as

                CA.TypeFunction _ parameterType lambdaModifier returnType:
                    try argument as

                        CA.ArgumentExpression argumentExpression:
                            [#

                                reference as List a -> Maybe a

                                argumentExpression as List Text

                                => returnType as Maybe Text

                            `parameterType` does not come from an annotation, but from inference
                            Because of this we can't use it to check*, but instead we need to infer and unify it with the argument

                            This is super important when the reference type contains type variables.

                            #]
                            fromExpression env argumentExpression >> andThen argumentType:

                            reason =
                                UnifyReason_CallArgument
                                    { reference = pos
                                    , argument = CA.argumentPos argument
                                    }

                            unify env pos reason argumentType parameterType >> andThen unifiedArgumentType:
                            applySubsToType returnType >> andThen actualReturnType:
                            isCompatibleWith env expectedType pos actualReturnType

                        CA.ArgumentMutable pos { ref, attrPath }:
                            xxx =
                                try lambdaModifier as
                                    LambdaNormal:
                                        return None
                                    LambdaConsuming:
                                        addCheckError pos [
                                            , "This variable is passed for mutation, but the function needs to consume it."
                                            , "TODO: wiki link to explain difference between consuming and mutating"
                                            ]

                            xxx >> andThen _:

                            try Dict.get ref env.instanceVariables as
                                Nothing:
                                    errorUndefinedVariable env pos ref >> andThen ty:
                                    return None

                                Just var:
                                    if not var.isMutable then
                                        ae =
                                            addError pos
                                                [ "You are trying to mutate variable `" .. (toHuman ref) .. "` but it was declared as not mutable!"
                                                , ""
                                                , "TODO [link to wiki page that explains how to declare variables]"
                                                ]
                                        ae >> andThen ty:
                                        return None

                                    else if typeContainsFunctions var.ty then
                                        # TODO what about constrained/unconstrained tyvars?
                                        addError pos [ "mutable arguments can't allow functions" ] >> andThen ty:
                                        return None

                                    else
                                        applyAttributePath env pos attrPath var.ty >> andThen ty:

                                        # TODO: this is pretty much copied from CA.ArgumentExpression above, would be nice to merge the two
                                        reason =
                                            UnifyReason_CallArgument
                                                { reference = pos
                                                , argument = CA.argumentPos argument
                                                }

                                        unify env pos reason ty parameterType >> andThen unifiedArgumentType:
                                        applySubsToType returnType >> andThen actualReturnType:
                                        isCompatibleWith env expectedType pos actualReturnType

                _:
                    addCheckError pos [
                        , "The code is trying to call this as if it was a function, but its type is: "
                        , typeToText env referenceType
                        ]

        CA.If pos { condition, true, false }:
            checkExpression env CoreTypes.bool condition >> andThen _:
            checkExpression env expectedType true >> andThen _:
            checkExpression env expectedType false


        CA.Try pos value patternsAndBlocks:
            fromExpression env value >> andThen inferredValueType:

            xxx =
              inferredValueType >> list_for patternsAndBlocks (pattern & block): patternTypeSoFar:

                fromPattern env pattern Dict.empty >> andThen patternOut:
                unify env patternOut.pos UnifyReason_TryPattern patternOut.ty patternTypeSoFar >> andThen unifiedPatternType:

                # TODO do I really need to apply subs here?
                applySubsToNonFreeTyvars env >> andThen env1:

                get (x: x.substitutions) >> andThen subs:
                ip =
                    insertPatternVars
                        { subs
                        , isParameter = False
                        , isRoot = False
                        }
                        patternOut.vars
                        env1

                ip >> andThen patternEnv:

                checkExpression patternEnv expectedType block >> andThen None:
                # TODO: totality check?

                return unifiedPatternType

            xxx >> andThen _:
            return None


        CA.Record pos maybeExtending attrValueByName:
            try expectedType as
                CA.TypeRecordExt _ _ _ attrTypeByName:
                    addCheckError pos [
                        , "Extensible record annotation is experimentally disabled [TODO link to why]"
                        ]

                CA.TypeRecord _ attrTypeByName:
                    try maybeExtending as
                        Nothing:
                            xxx =
                                None >> dict_for attrValueByName attrName: attrValue: None:
                                    try Dict.get attrName attrTypeByName as
                                        Nothing:
                                            addCheckError pos [
                                                , "This record has an attribute `" .. attrName .. "` which is not in the annotation."
                                                ]

                                        Just expectedAttrType:
                                            checkExpression env expectedAttrType attrValue

                            # The type must have all attributes that exist in the value
                            xxx >> andThen None:
                                None >> dict_for attrTypeByName attrName: attrType: None:
                                    try Dict.get attrName attrTypeByName as
                                        Nothing:
                                            addCheckError pos [
                                                , "This record is missing the attribute `" .. attrName .. "`"
                                                ]

                                        Just _:
                                            return None

                        Just extending:

                            # extending needs to be of expectedType
                            checkExpression env expectedType (CA.Variable pos extending) >> andThen None:

                            # the rest of the attributes, we need to ensure that they belong in expectedType with the correct type
                            None >> dict_for attrValueByName attrName: attrValue: None:
                                try Dict.get attrName attrTypeByName as
                                    Nothing:
                                        addCheckError pos [
                                            , "This record has an attribute `" .. attrName .. "` which is not in the annotation"
                                            ]

                                    Just expectedAttrType:
                                        checkExpression env expectedAttrType attrValue


                _:
                    addCheckError pos [
                        , "This is a record, but the annotation says that this should be a"
                        , typeToText env expectedType
                        ]

        CA.LetIn valueDef e:
            fromDefinition False valueDef env >> andThen env1:

            xxx =
                if CA.patternIsMutable valueDef.pattern and typeContainsFunctions expectedType then
                    addCheckError (CA.patternPos valueDef.pattern) [ "blocks that define mutables can't return functions" ]
                else
                    return None

            xxx
            >> andThen _:
            checkExpression env1 expectedType e



[# The general idea, and I don't know if it actually makes sense, is that we want a function that

1.  for a given expression tries to figure out its type
2.  collects type errors within the expression itself

#]
fromExpression as Env: CA.Expression: Monad Type =
    env: expression:

    try expression as
        CA.LiteralText pos l:
            return CoreTypes.text

        CA.LiteralNumber pos l:
            return CoreTypes.number

        CA.Variable pos { ref, attrPath }:
            try Dict.get ref env.instanceVariables as
                Nothing:
                    errorUndefinedVariable env pos ref

                Just var:
                    [#

                       The basic problem
                       -------------------

                       Consider:
                       ```
                       emptyList as [ item ]
                       emptyList = []

                       listOfText as [ Text ]
                       listOfText = "blah" :: emptyList

                       listOfBool as [ Bool ]
                       listOfBool = True :: emptyList
                       ```

                       `emptyList` is used twice with two different `item` types.

                       If fact, every single time `emptyList` is referenced, it can be used against a different `item` type.
                       To model this, every time we reference `emptyList` we replace `item` with a new variable, so that it has no constraints.
                       The type can have an arbitrary number of these "free type variables", so we create a new variable for each of them.


                       Ordering
                       --------

                       Now, let's say we are referencing `emptyList` before we could infer its type.

                       At this stage, the type of `emptyList` can only be a single free variable, so if we replace it with a new variable every time we
                       reference it we lose all information.

                       Because of this, we can't reference a variable before we know its type.
                       In turn this means that either we sort the definitions so that those that depend on others come afterwards, either we require annotations.

                       For recursive and mutually recursive functions, reordering is impossible but we can probably get away with not requiring annotations, but I'm not yet solid on this.
                       OTOH, if it becomes a pain, we *can* decide that Squarepants requires annotations for mutually recursive functions.


                       Non-free type variables
                       -----------------------

                       someValue =
                         is List a            -- *this* `a` is a free type variable

                         x =
                           is Maybe a         -- but this one isn't, because it must be the same as the one above!
                           fromSomewhereElse

                         [ x ]

                       The `a` in the annotation of `x` is NOT a free variable, but rather must be *the same* `a` used in the parent annotation!

                       In this case we do NOT instantiate a new variable!
                    #]
                    (replaceTypeVariablesWithNew var.freeTypeVariables var.ty) >> andThen varType:
                    [#

                       Dealing with attributes as a special case (rather than just as an access function)
                       allows Squarepants to mutate single attributes of a mutable record:

                       @someRecord.someAttribute += 1

                       @someRecord.someOtherAttribute.someNestedAttribute := "Squidward"

                    #]
                    applyAttributePath env pos attrPath varType

        CA.Constructor pos usr:
            try Dict.get usr env.constructors as
                Nothing:
                    errorUndefinedVariable env pos (CA.RefRoot usr)

                Just c:
                    replaceTypeVariablesWithNew (getFreeTypeVars Dict.empty c.type) c.type

        CA.Lambda pos param lambdaModifier body:

            fromPattern env param Dict.empty
            >> andThen patternOut:

            ip =
                insertPatternVars
                    { subs = Dict.empty
                    , isParameter = True
                    , isRoot = False
                    }
                    patternOut.vars
                    env

            ip >> andThen bodyEnv:
            fromExpression bodyEnv body >> andThen bodyType:

            # fromPattern can infer paramType only when destructuring some patterns, it's not reliable in general
            # fromBlock instead infers paramType fully, but will not apply the substitutions it creates
            # So we just pull out the substitutions and apply them to paramType
            applySubsToType patternOut.ty
            >> andThen refinedPatternOutTy:

#            if isMutable and typeContainsFunctions refinedPatternOutTy then
#                # TODO be a bit more descriptive, maybe name the arguments
#                errorTodo pos << "mutable args cannot be functions"
#
#            else

            CA.TypeFunction pos refinedPatternOutTy lambdaModifier bodyType
            >> return

        CA.Call pos reference argument:
            # first of all, let's get our children types
            fromExpression env reference >> andThen referenceType:
            fromArgument env argument >> andThen argumentType:
            # the type of the call itself is the return type of the lamba
            unifyFunctionOnCallAndYieldReturnType env reference referenceType argument argumentType

        CA.If pos ar:
            checkExpression env CoreTypes.bool ar.condition >> andThen _:
            get (x: x.substitutions) >> andThen s:
            (fromExpression env ar.true) >> andThen trueType:
            (fromExpression env ar.false) >> andThen falseType:
            unify env pos UnifyReason_IfBranches trueType falseType

        CA.Try pos value patternsAndBlocks:
            (fromExpression env value) >> andThen tryType:

            flags as CA.TyvarFlags = {
                , allowFunctions = True
                , allowUniques = False
                }

            newType pos flags >> andThen newBlockType:
            list_for patternsAndBlocks (fromPatternAndBlock env) ( tryType & newBlockType )
            >> andThen ( patternType & inferredBlockType ):
            return inferredBlockType

        CA.Record pos maybeExt attrValues:
            dict_map (k: fromExpression env) attrValues >> andThen attrTypes:
            try maybeExt as
                Nothing:
                    return << CA.TypeRecord pos attrTypes

                Just variableArgs:

                    flags = { allowFunctions = True, allowUniques = False }

                    # TODO here it would be easier to support an entire expression
                    fromExpression env (CA.Variable pos variableArgs) >> andThen ty_:
                    applySubsToType ty_ >> andThen ty:
                    newName identity >> andThen name:
                    unify env pos (UnifyReason_AttributeUpdate (Dict.keys attrTypes)) ty (CA.TypeRecordExt pos name flags attrTypes) >> andThen unifiedType:
                    return unifiedType

        CA.LetIn valueDef e:
            fromDefinition False valueDef env >> andThen env1:
            fromExpression env1 e >> andThen ty:
            if CA.patternIsMutable valueDef.pattern and typeContainsFunctions ty then
                addError (CA.patternPos valueDef.pattern) [ "blocks that define mutables can't return functions" ] >> andThen _:
                return ty
            else
                return ty




unifyFunctionOnCallAndYieldReturnType as Env: CA.Expression: Type: CA.Argument: Type: Monad Type =
    env: reference: referenceType: argument: callArgumentType:

    isMutation =
        try argument as
            CA.ArgumentMutable _ _: True
            CA.ArgumentExpression _: False

    try referenceType as

        CA.TypeFunction _ refArgumentType lambdaModifier refReturnType:
            pos =
                CA.expressionPos reference

            xxx =
                if isMutation and lambdaModifier == LambdaConsuming then
                    addCheckError pos [
                        , "You are passing this variable for mutation, but the function needs to consume it!"
                        ]
                else
                    return None

            xxx >> andThen _:

            reason =
                UnifyReason_CallArgument
                    { reference = pos
                    , argument = CA.argumentPos argument
                    }

            unify env pos reason refArgumentType callArgumentType
            >> andThen unifiedArgumentType:

            applySubsToType refReturnType

        CA.TypeVariable pos name flags:


            todo ",mnas,kjasdf"

            newType pos flags
            >> andThen returnType:

            modifier =
                if not isMutation and CA.typeIsMutable callArgumentType then
                    LambdaConsuming
                else
                    LambdaNormal

            ty =
                CA.TypeFunction pos callArgumentType LambdaNormal returnType

            unify env pos (UnifyReason_IsBeingCalledAsAFunction pos referenceType) referenceType ty
            >> andThen _:

            applySubsToType returnType

        CA.TypeAlias pos _ ty:
            unifyFunctionOnCallAndYieldReturnType env reference ty argument callArgumentType

        _:
            addError (CA.expressionPos reference)
                [ "This is being called like a function, but its type is"
                , ""
                , typeToText env referenceType
                ]


fromPatternAndBlock as Env: ( CA.Pattern & CA.Expression ): ( Type & Type ): Monad ( Type & Type ) =
    env: ( pattern & block ): ( patternTypeSoFar & blockTypeSoFar ):
    fromPattern env pattern Dict.empty >> andThen patternOut:
    unify env patternOut.pos UnifyReason_TryPattern patternOut.ty patternTypeSoFar >> andThen unifiedPatternType:

    # TODO do I really need to apply subs here?
    applySubsToNonFreeTyvars env >> andThen env1:

    get (x: x.substitutions) >> andThen subs:
    ip =
        insertPatternVars
            { subs
            , isParameter = False
            , isRoot = False
            }
            patternOut.vars
            env1
    ip >> andThen patternEnv:

    fromExpression patternEnv block >> andThen blockType:
    # TODO pos should be the block's last statement
    unify env patternOut.pos (UnifyReason_TryBlock block) blockTypeSoFar blockType >> andThen unifiedBlockType:
    return ( unifiedPatternType & unifiedBlockType )


fromArgument as Env: CA.Argument: Monad Type =
    env: argument:

    try argument as

        CA.ArgumentExpression expr:
            fromExpression env expr

        CA.ArgumentMutable pos { ref, attrPath }:

            try Dict.get ref env.instanceVariables as

                Nothing:
                    errorUndefinedVariable env pos ref >> andThen ty:
                    return (CA.TypeMutable Pos.N ty)

                Just var:
                    if not var.isMutable then

                        addError pos
                            [ "You are trying to mutate variable `" .. toHuman ref .. "` but it was declared as not mutable!"
                            , ""
                            , "TODO [link to wiki page that explains how to declare variables]"
                            ]
                        >> andThen ty:

                        return (CA.TypeMutable Pos.N ty)

                    else if typeContainsFunctions var.ty then
                        # TODO what about constrained/unconstrained tyvars?
                        addError pos [ "mutable arguments can't allow functions" ]

                        >> andThen ty:
                        return (CA.TypeMutable Pos.N ty)

                    else
                        applyAttributePath env pos attrPath var.ty


[# Patterns are special because they are the one way to **add variables to the env**.

When we have information about the type of these variables we want to add

                (because they are used in a block, for example)

Usually these variables are used in a block, and ide

Often we have important information about these variables, be

If we have information about the type of these variables ( because they are used in a block)

              There are two ways new variables can be added to the env: via definitions ("x = ...") or as function arguments ("x: ...")

I'm not mathematically certain of this, but I think that as long as we require recursive definitions to be annotated, we can fully infer the type of defined variables from their body, ie _before_ we need to add them to the env.

This is good because it means that we never need to apply substitutions to defined variables in the env.

Function arguments however can't be inferred before we have actually used them in the function body.
(We can do some inference if we are destructuring a pattern, but most of the times that's not enough)

This means that once we have gone through the body, we do need to apply the new substitutions to the placeholder type we gave to the parameter before inspecting the block.

---

Each way to introduce variables has one way to _define_ the type.

This is not the same as validation.
Validation determines if it is ok or not

  - fromStatement
    var =
    ...

        type is entirely determined by the definition

```
  (x & y) =
      (1 & 2)

  pattern: (a & b)
  block: (Int & Int)

  devo unificarlo e poi applicare le subs all'env?

  Do I need to apply substitutions to the env?
```

  - fromParameter
    var:
    ...

        type is entirely determined by **subsequent usage**

        --> We add them as they are, then **explicitly apply substitutions**

  - fromPatternAndBlock
    try x as
    prev:
    ...
    var:
    ...

        type is entirely determined by the previous patterns and the statement

#]
alias PatternVar = {
    , pos as Pos
    , isMutable as Bool
    , type as Type
    , isAnnotated as Bool
    }

alias PatternVars =
    Dict Name PatternVar


alias PatternOut =
    { vars as PatternVars
    , pos as Pos
    , ty as Type
    , isFullyAnnotated as Bool
    }



fromMaybeAnnotation as Env: Pos: Bool: Maybe Name: Maybe Type: Dict Name PatternVar: Monad PatternOut =
    env: pos: isMutable: maybeName: maybeAnnotation: vars:

    isAnnotated =
        maybeAnnotation /= Nothing

    makeNew as Monad Type =

        flags as CA.TyvarFlags = {
            , allowFunctions = True
            , allowUniques = False
            }


        newType pos flags >> andThen ty:
        if isMutable then
            return << CA.TypeMutable pos ty
        else
            return ty

    makeType =
        try maybeAnnotation as
            Nothing:
                makeNew

            Just type:
                try Compiler/ExpandTypes.expandAnnotation env.types type as
                    Err e:
                        insertError e >> andThen None:
                        makeNew

                    Ok t:
                        return t

    makeType >> andThen type:

    checkMutability =
        if isMutable /= CA.typeIsMutable type then
            addCheckError pos [
                # TODO this error sucks
                , "type annotation and variable declaration have different mutability"
                ]
        else
            return None

    checkMutability
    >> andThen None:

    newVars =
        try maybeName as
            Nothing: vars
            Just name: Dict.insert name { pos, type, isMutable, isAnnotated } vars

    {
    , vars = newVars
    , pos
    , ty = type
    , isFullyAnnotated = isAnnotated
    }
    >> return


fromPattern as Env: CA.Pattern: PatternVars: Monad PatternOut =
    env:
    pattern:
    # This is used only when fromPattern calls itself recursively
    vars_:

    vars as Dict Name PatternVar =
        vars_

    try pattern as

        CA.PatternAny pos isMutable maybeName maybeAnnotation:
            fromMaybeAnnotation env pos isMutable maybeName maybeAnnotation vars

        CA.PatternLiteralNumber pos literal:
            return << { vars, pos, ty = CoreTypes.number, isFullyAnnotated = True }

        CA.PatternLiteralText pos literal:
            return << { vars,  pos, ty = CoreTypes.text, isFullyAnnotated = True }

        CA.PatternConstructor pos usr args:
            constructorTyM =
                try Dict.get usr env.constructors as
                    Nothing:
                        # TODO still add all variables defined in the args, otherwise there will
                        # missing variables that will trigger misleading "undefined variable" errors
                        # (ie, use unifyConstructorWithItsArgs anyway)
                        errorUndefinedVariable env pos (CA.RefRoot usr)

                    Just c:
                        replaceTypeVariablesWithNew (getFreeTypeVars Dict.empty c.type) c.type

            constructorTyM >> andThen constructorTy:

            p as UnifyConstructorWithItsArgsParams = {
                , env
                , usr
                , pos
                , ty = constructorTy
                , args
                , argIndex = 0
                , vars
                , isFullyAnnotated = True
                }

            unifyConstructorWithItsArgs p >> andThen ( patternVars & patternTy & isFullyAnnotated ):
            return << { vars = patternVars, pos, ty = patternTy, isFullyAnnotated }

        CA.PatternRecord pos attrs:

            # Ok, I want local aliases so I can annotate the horror below

            blah as Name: CA.Pattern: (PatternVars & Dict Name Type & Bool): Monad (PatternVars & Dict Name Type & Bool) =
                name: pa: ( varsX & attrTypes & annotatedSoFar):
                fromPattern env pa varsX >> andThen paOut:
                return ( paOut.vars & Dict.insert name paOut.ty attrTypes & paOut.isFullyAnnotated and annotatedSoFar )

            dict_for attrs blah ( vars & Dict.empty & True) >> andThen ( vars1 & attrTypes & isFullyAnnotated ):
            #
            # For the time being, if you unpack a record, you must unpack ALL of it
            # This is IMHO better for readability, but only one way to find out...
            #
            #newName identity >> andThen extName:
            return << { vars = vars1, pos, ty = CA.TypeRecord pos attrTypes, isFullyAnnotated }


alias UnifyConstructorWithItsArgsParams =
    { env as Env
    , usr as Meta.UniqueSymbolReference
    , pos as Pos
    , ty as Type
    , args as List CA.Pattern
    , argIndex as Int
    , vars as PatternVars
    , isFullyAnnotated as Bool
    }


unifyConstructorWithItsArgs as UnifyConstructorWithItsArgsParams: Monad ( PatternVars & Type & Bool ) =
    p:
    try p.ty & p.args as
        # Argument needed, argument given
        ( CA.TypeFunction _ from _ to & head :: tail ):
            (fromPattern p.env head p.vars) >> andThen pa:
            { vars, pos, ty, isFullyAnnotated } = pa
            (unify p.env pos (UnifyReason_ConstructorArgument p) from ty) >> andThen unifiedFrom:
            unifyConstructorWithItsArgs
                { p with
                    , argIndex = p.argIndex + 1
                    , ty = to
                    , args = tail
                    , vars = vars
                    , isFullyAnnotated = isFullyAnnotated and p.isFullyAnnotated
                }

        # Error: Argument needed but not given!
        ( CA.TypeFunction _ from _ to & [] ):
            # TODO tell how many are needed and how many are actually given
            (addError p.pos [ "Type constructor " .. toHuman p.usr .. " is missing argument #" .. Text.fromNumber p.argIndex ]) >> andThen ety:
            return ( p.vars & ety & p.isFullyAnnotated )

        # No arguments needed, no arguments given
        ( _ & [] ):
            get (x: x.substitutions) >> andThen subs:
            # TODO should I apply subs here?
            return ( p.vars & p.ty & p.isFullyAnnotated )

        # Error: no argument needed, but argument given!
        ( _ & head :: tail ):
            # TODO tell how many are needed and how many are actually given
            (addError p.pos [ "Type constructor " .. toHuman p.usr .. " has too many args" ]) >> andThen ety:
            return ( p.vars & ety & p.isFullyAnnotated )



#
# Unification
#


union UnifyReason =
    , UnifyReason_AnnotationSimple
    , UnifyReason_AnnotationVsBlock CA.Pattern CA.Annotation CA.Expression
    , UnifyReason_DefBlockVsPattern
    , UnifyReason_CallArgument { reference as Pos, argument as Pos }
    , UnifyReason_IsBeingCalledAsAFunction Pos Type
    , UnifyReason_IfCondition
    , UnifyReason_IfBranches
    , UnifyReason_TryPattern
    , UnifyReason_TryBlock CA.Expression
    , UnifyReason_ConstructorArgument UnifyConstructorWithItsArgsParams
    , UnifyReason_AttributeAccess Name
    , UnifyReason_AttributeUpdate (List Name)
    , UnifyReason_Override
    , UnifyReason_IsLambda


union UnifyError =
    , IncompatibleTypes
    , IncompatibleMutability
    , IncompatibleRecords { aOnly as List Name, bOnly as List Name, bothUnified as List Name }
    , Cycle Name
    # TODO should be List RejectFunction but am not sure we actually need it
    , NonFunctionContainsFunction (List None)
    , OkThisIsActuallyPossible
    , NI Text
    , SubstitutingAnnotation Name


unifyErrorToText as UnifyError: Text =
    ue:
    try ue as
        IncompatibleTypes:
            "The two types are incompatible."

        IncompatibleMutability:
            "The mutability does not match."

        IncompatibleRecords args:
            "The record types are not compatible" .. toHuman args

        Cycle name:
            "There is a cyclic dependency on " .. name

        NonFunctionContainsFunction rejectFunctions:
            "NonFunction can't contain functions: " .. toHuman rejectFunctions

        OkThisIsActuallyPossible:
            "OkThisIsActuallyPossible?"

        NI str:
            "Not Implemented: " .. str

        SubstitutingAnnotation name:
            "SubstitutingAnnotation: " .. name



unify as Env: Pos: UnifyReason: Type: Type: Monad Type =
    env: pos: reason: a: b:

    get (x: x.typeClashesByPlaceholderId) >> andThen tc:
    if tc /= Nothing then
        todo "typeClashesByPlaceholderId NOT EMPTY!"

    else
        m_update (s: { s with typeClashesByPlaceholderId = Just Dict.empty }) >> andThen _:
        unify_ env reason pos a b >> andThen unifiedType:
        popClashingtypes >> andThen typeClashes:
        if typeClashes == Dict.empty then
            return unifiedType

        else
            # there were type clashes, so turn them into errors
            errorIncompatibleTypes env reason pos unifiedType typeClashes >> andThen _:
            return unifiedType




[# Unification is always successful: if two types can't be unified, then the unification error is added to the state and a brand new type variable is returned in place of the clashing types.

NOTE: the t1 should be the type with an actual position in the code because its position is the one that will be used.

TODO: when do I refine the environment?
----> never, ideally: if it's a definition we know its value after inferring its body, if it's a lambda parameter we know it when we stop using it.
Keeping the subs up to date should be enough

Are substitutions interesting only against a specific environment instance?

#]
unify_ as Env: UnifyReason: Pos: Type: Type: Monad Type =
    env: reason: pos1: t1: t2:
    try t1 & t2 as
        CA.TypeAlias pos _ aliased & _:
            unify_ env reason pos aliased t2

        _ & CA.TypeAlias _ _ aliased:
            unify_ env reason pos1 t1 aliased

        CA.TypeConstant pos ref1 args1 & CA.TypeConstant _ ref2 args2:
            if ref1 /= ref2 then
                unifyError pos1 IncompatibleTypes t1 t2

            else

                fold =
                    arg1: arg2:
                    unify_ env reason pos arg1 arg2

                # TODO is this the correct place where to check arity?
                (list_map2 (unify_ env reason pos) args1 args2) >> andThen argTypes:
                get (x: x.substitutions) >> andThen subs:
                argTypes
                    >> List.map (replaceTypeVariables subs)
                    >> CA.TypeConstant pos ref1
                    >> return

        CA.TypeVariable pos v1_name flags1 & CA.TypeVariable _ v2_name flags2:
            if v1_name == v2_name then
                return t1

            else
                get (x: x.substitutions) >> andThen subs:
                try ( Dict.get v1_name subs & Dict.get v2_name subs ) as
                    ( Just sub1 & Just sub2 ):
                        (unify_ env reason pos1 sub1 sub2) >> andThen v:
                        # I think override here is False because it was used to propagate NonFunction in one of the attempted implementations
                        (addSubstitution env "vv1" pos reason v1_name v) >> andThen _:
                        (addSubstitution env "vv2" pos reason v2_name v) >> andThen subbedTy:
                        return subbedTy

                    ( Nothing & Just sub2 ):
                        addSubstitution env "vv3" pos reason v1_name t2

                    _:
                        addSubstitution env "vv4" pos reason v2_name t1

        CA.TypeVariable pos name1 flags1 & _ :
            addSubstitution env "vl" pos reason name1 t2

        _ & CA.TypeVariable pos name2 flags2 :
            addSubstitution env "vr" pos reason name2 t1

        CA.TypeFunction pos a_from a_fromIsMutable a_to & CA.TypeFunction _ b_from b_fromIsMutable b_to :
            if a_fromIsMutable /= b_fromIsMutable then
                unifyError pos IncompatibleMutability t1 t2

            else
                (unify_ env reason pos a_from b_from) >> andThen unified_from:
                get (x: x.substitutions) >> andThen subs_:
                [# Without the replacement here, a function annotation will produce circular substitutions

                   id a =
                       is a: a
                       a

                #]
                (unify_ env reason pos (replaceTypeVariables subs_ a_to) (replaceTypeVariables subs_ b_to)) >> andThen unified_to:
                get (x: x.substitutions) >> andThen subs:
                CA.TypeFunction pos (replaceTypeVariables subs unified_from) a_fromIsMutable (replaceTypeVariables subs unified_to)
                    >> return

        CA.TypeRecord _ a_attrs & _:
            unifyRecords env reason pos1 a_attrs t2

        CA.TypeRecordExt _ a_ext a_flags a_attrs & _:
            unifyRecordsExt env reason pos1 a_ext a_flags a_attrs t2

        CA.TypeMutable _ a & CA.TypeMutable _ b:
            unify_ env reason pos1 a b

#        CA.TypeMutable _ (CA.TypeVariable _ name) & CA.TypeRecord _ ext attrs:


        _:
            unifyError pos1 IncompatibleTypes t1 t2


[# TODO Rename to type clash?
#]
unifyError as Pos: UnifyError: Type: Type: Monad Type =
    pos: error: t1: t2:
    newName identity >> andThen name:
    insertTypeClash name t1 t2 error >> andThen None:
    return << CA.TypeVariable pos name { allowFunctions = True, allowUniques = False }



#
# Add substitutions
#


isAnnotation as Name: Bool =
    n:
    Text.toNumber n == Nothing


addSubstitution as Env: Text: Pos: UnifyReason: Name: Type: Monad Type =
    env: debugCode: pos: reason: name: rawTy:
    (applySubsToType rawTy) >> andThen ty:
    if isAnnotation name then
        try ty as
            CA.TypeVariable _ subName flags:

                todo "lasjflkasflkajf"

                # HACK
                # if the two variables are the same, we probably shouldn't even get here
                if subName == name then
                    return ty

                else if isAnnotation subName then
                    unifyError pos (SubstitutingAnnotation name) (CA.TypeVariable pos name flags) ty

                else
                    addSubstitution env (debugCode .. " SWITCH") pos reason subName (CA.TypeVariable pos name flags)

            _:
                unifyError pos (SubstitutingAnnotation name) (CA.TypeVariable pos name (todo "lkjasflkjjahfljashf")) ty

    else if typeHasTyvar name ty then
        # TODO This feels a bit like a hacky work around.
        # Maybe it's because I don't call applySubsToType enough before calling unify?
        # Then again, it feels more robust?
        # Too much feeling, not enough understanding.
        if typeIsTyvar name ty then
            return ty

        else
            unifyError pos (Cycle name) (CA.TypeVariable pos name (todo "nzknas,mccnasdf")) ty

    else
        (checkNonFunction env name ty) >> andThen nonFunction:
        { freeVarsToFlag } = nonFunction
        (flagFreeVars freeVarsToFlag) >> andThen _:
        get (x: x.substitutions) >> andThen subs:
        try Dict.get name subs as
            [#
               The tyvar has already been substituted
               This means that it has been already applied to all other subs and `name` does not appear in the subs.

               Is this enough to exclude infinite calls between unify_ and addSubstitution?
            #]
            Just sub:
                unify_ env reason pos ty sub

            Nothing:
                # apply new substitution to all old substitutions
                state:
                    ( ty & { state with substitutions =
                            state.substitutions
                                >> Dict.map (k: replaceTypeVariables (Dict.singleton name ty))
                                >> Dict.insert name ty
                    })


typeIsTyvar as Name: Type: Bool =
    name: ty:
    try ty as
        CA.TypeVariable _ n _:
            n == name

        _:
            False


checkNonFunction as Env: Name: Type: Monad { freeVarsToFlag as List Name } =
    env: name: ty:

    nope =
        { freeVarsToFlag = [] }

    get (x: x.nonFnTyvars) >> andThen nonFnTyvars:
    try Dict.get name nonFnTyvars as
        Nothing:
            return nope

        Just rejectReasons:
            # type should not contain function
            if typeContainsFunctions ty then
                [# TODO how do I talk about this type?

                   "The type of `f`"?

                #]
                (errorTodo (Pos.I 26) << "type `" .. name .. "` should not contain functions, but is " .. typeToText env ty) >> andThen _:
                return nope

            else
                # blah!
                # TODO constrained vars inside ty should all reject functions
                # TODO non-constraned vars should be flagged with reject function
                return nope


flagFreeVars as List Name: Monad None =
    names:
    # TODO!!!!
    return None



#
# Various helpers
#


typeContainsFunctions as Type: Bool =
    ty:
    try ty as
        CA.TypeConstant _ _ args:
            List.any typeContainsFunctions args

        CA.TypeVariable _ _ _:
            False

        CA.TypeFunction _ from fromIsMutable to:
            True

        CA.TypeAlias _ path t:
            typeContainsFunctions t

        CA.TypeMutable _ t:
            typeContainsFunctions t

        CA.TypeRecord _ attrs:
            attrs
            >> Dict.values
            >> List.any typeContainsFunctions

        CA.TypeRecordExt _ _ _ attrs:
            attrs
            >> Dict.values
            >> List.any typeContainsFunctions



typeHasTyvar as Name: Type: Bool =
    n: ty:

    try ty as
        CA.TypeVariable pos name _:
            n == name

        CA.TypeFunction _ from fromIsMutable to:
            typeHasTyvar n from or typeHasTyvar n to

        CA.TypeConstant pos ref args:
            List.any (typeHasTyvar n) args

        CA.TypeAlias _ path t:
            typeHasTyvar n t

        CA.TypeMutable _ t:
            typeHasTyvar n t

        CA.TypeRecord pos attrs:
            List.any (typeHasTyvar n) (Dict.values attrs)

        CA.TypeRecordExt pos name flags attrs:
            n == name or List.any (typeHasTyvar n) (Dict.values attrs)



attributeAccess as Env: Pos: Name: Type: Monad Type =
    env: pos: attributeName: recordType:

        # Unless this is a function parameter, we should know the full type already!
        maybeAttrType =
            try recordType as
                CA.TypeRecord _ attrs:
                    Dict.get attributeName attrs

                CA.TypeRecordExt _ _ _ attrs:
                    Dict.get attributeName attrs

                CA.TypeMutable p (CA.TypeRecord _ attrs):
                    Dict.get attributeName attrs
                    >> Maybe.map (CA.TypeMutable p)

                CA.TypeMutable p (CA.TypeRecordExt _ _ _ attrs):
                    Dict.get attributeName attrs
                    >> Maybe.map (CA.TypeMutable p)

                _:
                    Nothing

        try maybeAttrType as
            Just attrType:
                return attrType

            Nothing:
                newName identity >> andThen extName:

                flags as CA.TyvarFlags = {
                    , allowFunctions = True
                    , allowUniques = False
                    }

                newType pos flags >> andThen attrType:

#                try recordType as
#                    CA.TypeMutable _


                re as Type =
                    CA.TypeRecordExt (Pos.I 2) extName flags (Dict.singleton attributeName attrType)

                unify env pos (UnifyReason_AttributeAccess attributeName) recordType re >> andThen _:
                return attrType




applyAttributePath as Env: Pos: List Name: Type: Monad Type =
    env: pos: attrPath:

    list_for attrPath (attributeAccess env pos)


alias InsertPatternVarsPars =
    { subs as Subs
    , isParameter as Bool
    , isRoot as Bool
    }

insertPatternVars as InsertPatternVarsPars: PatternVars: Env: Monad Env =
    pars: vars:
    dict_for vars (insertPatternVar pars)


insertPatternVar as InsertPatternVarsPars: Name: PatternVar: Env: Monad Env =
    pars: name: patternVar: env:

#    { subs, isParameter, isMutable, isRoot } = pars
#    { pos, type, isAnnotated } = patternVar

    refinedTy =
        if patternVar.isAnnotated then
            patternVar.type
        else
            replaceTypeVariables pars.subs patternVar.type

    ref =
        if pars.isRoot then
            CA.RefRoot << Meta.USR env.currentModule name
        else
            CA.RefBlock name

    { # TODO env with
    , currentModule = env.currentModule
    , meta = env.meta
    , constructors = env.constructors
    , types = env.types
    # TODO yeah, type check messes this ^ ?

    , nonAnnotatedRecursives =
        if patternVar.isAnnotated then
            # TODO use shorthand
            env.nonAnnotatedRecursives
        else
            # TODO use shorthand
            Dict.insert name patternVar.pos env.nonAnnotatedRecursives

    , instanceVariables =
        Dict.insert ref
            { definedAt = patternVar.pos
            , ty = refinedTy
            , isMutable = patternVar.isMutable
            , freeTypeVariables =
                if patternVar.isMutable or pars.isParameter then
                    [#
                       Mutables can mutate to any value of a given specific type, so they can't be polymorphic.

                       i.e: `x = Nothing` has type `Maybe a`, but once I mutate it to `@x := Just 1` its type would
                       change to `Maybe Number` and we don't want that.

                       Function parameters instead are **totally determined by how they are used in the function's body**,
                       so they are always added to the scope as non-free type variables.
                       If we turned them into free type variables, the block would not be able to constrain them.

                       Defined instance values are **totally determined by their definition**, how they are used must not
                       affect their type, so each time they are used they can be used with different free type vars.

                       TODO: what about instance variables added by try..as patterns?
                    #]
                    Dict.empty

                else
                    getFreeTypeVars env.nonFreeTyvars refinedTy
            }
            # TODO use shorthand
            env.instanceVariables
    , nonFreeTyvars =
        if pars.isParameter then
            [#

               Within the function body, the type of the parameter must be considered non-free!

               Consider:

                   x q =
                         p = q
                         p

               if the type of `q` remains free, then every time we use `p`, `p` will get an entirely new type.

            #]
            Dict.for (typeTyvars refinedTy) Dict.insert env.nonFreeTyvars

        else
            env.nonFreeTyvars
    }
        >> return


applySubsToNonFreeTyvars as Env: Monad Env =
    env:

    get (x: x.substitutions) >> andThen subs:
    [#
       Consider:

       x q =
             { first } = q
             first

       When we see that `q` must be a record, we need to replace its type.
       However, because the type of `q` is a constrained type variable, we need to make sure that the type(s)
       we use for the records are also constrained!

    #]

    meh as Name: Dict Name CA.TyvarFlags: Dict Name CA.TyvarFlags =
        typeVarName: constrainedVars:
        try Dict.get typeVarName subs as
            Nothing:
                constrainedVars

            Just ty:
                Dict.for (typeTyvars ty) (n: p: Dict.insert n p) constrainedVars

    return { env with nonFreeTyvars = List.for (Dict.keys env.nonFreeTyvars) meh env.nonFreeTyvars }



replaceTypeVariablesWithNew as Dict Name CA.TyvarFlags: Type: Monad Type =
    freeTypeVariables: type:

    if freeTypeVariables == Dict.empty then
        return type

    else
        generateNewTypeVariables freeTypeVariables >> andThen newTypeByOldType:
        return << replaceTypeVariables newTypeByOldType type


generateNewTypeVariables as Dict Name CA.TyvarFlags: Monad Subs =
    tyvarByName:

    apply as Name: CA.TyvarFlags: Subs: Monad Subs =
        name0: arg: subs:
        newName identity >> andThen name1:
        return << Dict.insert name0 (CA.TypeVariable (Pos.I 11) name1 { allowFunctions = True, allowUniques = False }) subs

    dict_for tyvarByName apply Dict.empty


#FLAG


replaceTypeVariables as Subs: Type: Type =
    subs: ty:
    try ty as
        CA.TypeConstant pos ref args:
            CA.TypeConstant pos ref (List.map (replaceTypeVariables subs) args)

        CA.TypeVariable _ name flags:
            try Dict.get name subs as
                Just substitutionType:
                    # addSubstitution always applies all subs to each sub's type, so we don't need to apply them here
                    substitutionType

                Nothing:
                    # no substitution, return the type as-is
                    ty

        CA.TypeFunction pos from fromIsMutable to:
            CA.TypeFunction pos
                (replaceTypeVariables subs from)
                fromIsMutable
                (replaceTypeVariables subs to)

        CA.TypeAlias pos path t:
            CA.TypeAlias pos path (replaceTypeVariables subs t)

        CA.TypeMutable pos t:
            CA.TypeMutable pos (replaceTypeVariables subs t)

        CA.TypeRecord pos attrs:
            CA.TypeRecord pos (Dict.map (name: replaceTypeVariables subs) attrs)

        CA.TypeRecordExt pos name flags attrs:
            try Dict.get name subs as
                Nothing:
                    CA.TypeRecordExt pos name flags (Dict.map (name: replaceTypeVariables subs) attrs)

                Just (CA.TypeVariable p n f):
                    CA.TypeRecordExt pos n f (Dict.map (name: replaceTypeVariables subs) attrs)

                Just (CA.TypeRecordExt _ name flags attrs2):
                    # attrs2 is the sub, so we give precedence to it
                    Dict.join attrs2 attrs
                    >> Dict.map (name: replaceTypeVariables subs)
                    >> CA.TypeRecordExt pos name flags

                Just what:
                    log "what" (toHuman what)
                    todo "replacing record extension with non-var"


addCheckError as Pos: [Text]: Monad None =
    pos: message:
    insertError (Error.Simple pos _: message)


addError as Pos: List Text: Monad Type =
    pos: message:
    addErrorWithEEnv pos (_: message)


addErrorWithEEnv as Pos: (Error.Env: List Text): Monad Type =
    pos: messageConstructor:
    (insertError (Error.Simple pos messageConstructor)) >> andThen None:

    flags as CA.TyvarFlags = {
        , allowFunctions = True
        , allowUniques = True
        }

    newType pos flags



#
# Errors
#
[# TODO this should go somewhere else #]


splitName as Text: ( Maybe Text & Text ) =
    s:
    try Text.split "." s as
        moduleName :: valueName :: []:
            ( Just moduleName & valueName )

        _:
            ( Nothing & s )


errorUndefinedVariable as Env: Pos: CA.Ref: Monad Type =
    env: pos: ref:

    addErrorWithEEnv pos errorEnv:

    onLocal = name:
        try Dict.get name env.nonAnnotatedRecursives as
            Just defPos:
                [ "To use function `" .. name .. "` recursively, you need to add a type annotation to its definition."
                , ""
                , "This is a limit of the compiler, not sure when I'll have the time to fix it."
                ]

            Nothing:
                [ "Undefined value: " .. name
                , ""
                , "I can't see a definition for `" .. name .. "` anywhere, so I don't know what it is."
                ]

    try ref as
        CA.RefBlock name:
            onLocal name

        CA.RefRoot (Meta.USR umr name):
            if umr == env.currentModule then
                onLocal name
            else
                Meta.UMR source path =
                    umr

                # Decide whether the module exists or not
                # TODO we should not rely on errorEnv for this test, but on whatever MakeCanonical is using
                # Better: store the original?
                try Dict.get path errorEnv.moduleByName as
                    Just mod:
                        [ "Module `" .. path .. "` from source `" .. toHuman source .. "` does not seem to expose a variable called `" .. name .. "`."
                        ]

                    Nothing:
                        [ "The code references a `" .. path .. "." .. name .. "` with source `" .. toHuman source
                        , "However, I can't find any module with that path and source."
                        ]



errorIncompatibleTypes as Env: UnifyReason: Pos: Type: Dict Name TypeClash: Monad Type =
    env: reason: pos_whatever: unifiedType: clashes:
    try reason as
        UnifyReason_CallArgument pos:

            makeError = eenv:

                { location, block } =
                    Error.posToHuman eenv pos.reference

                [ "This expression cannot be used as argument to this function:"
                , ""
                , block
                , ""
                , "the argument type seems to be: "
                , clashToTexts env
                    { typeSeemsToBe = "The argument type seems to be"
                    , type1_is = "The functon expects:"
                    , type2_is = "But the actual argument is:"
                    , unifiedType = unifiedType
                    , clashes = clashes
                    }
                ]

            addErrorWithEEnv pos.argument makeError

        UnifyReason_TryBlock block:

            makeError = eenv:
                [ "This try..as block produces a different type than the blocks preceding it."
                , ""
                , clashToTexts env
                    { typeSeemsToBe = "The block type seems to be"
                    , type1_is = "The previous block(s) produce:"
                    , type2_is = "But this block produces:"
                    , unifiedType = unifiedType
                    , clashes = clashes
                    }
                ]

            addErrorWithEEnv (CA.expressionPos block) makeError

        UnifyReason_AnnotationVsBlock pattern annotation body:
            headerPos =
                CA.patternPos pattern

            lastStatementPos =
                body
                    >> CA.skipLetIns
                    >> CA.expressionPos

            name =
                pattern
                    >> CA.patternNames
                    >> Dict.keys
                    >> Text.join ", "

            makeError = eenv:

                { location, block } =
                    Error.posToHuman eenv headerPos

                [ "The definition of " .. name .. " does not match the annotation:"
                , ""
                , block
                , ""
                , clashToTexts env
                    { typeSeemsToBe = "The produced type seems to be"
                    , type1_is = "The annotation says:"
                    , type2_is = "But this definition produces:"
                    , unifiedType = unifiedType
                    , clashes = clashes
                    }
                ]

            addErrorWithEEnv lastStatementPos makeError

        UnifyReason_IsBeingCalledAsAFunction pos referenceType:
            addError pos
                [ "This expression is being called as if it was a function, but its type is:"
                , ""
                , typeToText env referenceType
                ]

        _:
            pos =
                pos_whatever

            title =
                try reason as
                    UnifyReason_AnnotationSimple:
                        "The type is not compatible with the annotation"

                    UnifyReason_DefBlockVsPattern:
                        "The definition block cannot be unpacked into the pattern"

                    UnifyReason_IfCondition:
                        "The expression inside `if ... :` should always be a Bool"

                    UnifyReason_IfBranches:
                        "The branches of an `if` should produce the same type of value"

                    UnifyReason_TryPattern:
                        "try..as patterns should have the same type"

                    UnifyReason_ConstructorArgument p:
                        "Argument " .. Text.fromNumber p.argIndex .. " to type constructor " .. (toHuman p.usr) .. " does not match the constructor definition"

                    UnifyReason_AttributeAccess attrName:
                        "You are trying to access the ." .. attrName .. " attribute"

                    UnifyReason_AttributeUpdate attrNames:
                        "You are trying to update the " .. Text.join ", " attrNames .. " attributes"

                    UnifyReason_Override:
                        "this is addSubstitution running a UnifyReason_Override, I don't know what I'm doing"

                    UnifyReason_IsLambda:
                        "this is a function, and its type should reflect that"

                    _:
                        todo << toHuman reason .. " should not even get here"

            [ title
            , ""
            , "The type seems to be something like"
            , clashToTexts env
                { typeSeemsToBe = "type seems to be"
                , type1_is = "t1 is:"
                , type2_is = "but t2 is:"
                , unifiedType = unifiedType
                , clashes = clashes
                }
            ]
                >> addError pos_whatever


alias ClashToTextsParams =
    { typeSeemsToBe as Text
    , type1_is as Text
    , type2_is as Text
    , unifiedType as Type
    , clashes as Dict Name TypeClash
    }


clashToTexts as Env: ClashToTextsParams: Text =
    env: params:
    try params.unifiedType as
        CA.TypeVariable p unifiedTypeName f:
            params.clashes
                >> Dict.toList
                >> List.concatMap
                    (( clashPlaceholderName & clash ):
                        [ params.type1_is
                        , ""
                        , "  " .. typeToText env clash.t1
                        , ""
                        , params.type2_is
                        , ""
                        , "  " .. typeToText env clash.t2
                        , ""
                        , unifyErrorToText clash.err
                        ]
                    )
                >> Text.join "\n"

        _:
            info =
                # TODO the layout should change depending on the reason, not only the title
                [ params.typeSeemsToBe
                , typeToText env params.unifiedType
                , ""
                , "However I can't reconcile the following:"
                ]

            clashToError = ( name & clash ):
                # as ( Id & TypeClash ): List Text
                [ ""
                , "* `" .. name .. "`"
                , ""
                , "  " .. params.type1_is
                , "  " .. typeToText env clash.t1
                , ""
                , "  " .. params.type2_is
                , "  " .. typeToText env clash.t2
                , ""
                , "  " .. unifyErrorToText clash.err
                ]

            clashErrors =
                params.clashes
                    >> Dict.toList
                    >> List.concatMap clashToError

            [ info
            , clashErrors
            ]
                >> List.concat
                >> Text.join "\n"


errorTodo as Pos: Text: Monad Type =
    pos: message:
    addError pos [ message ]





#
#
# Records
#
#
onlyBothOnly as Dict key a: Dict key b: Dict key a & Dict key (a & b) & Dict key b =
    da: db:

    onAOnly =
        key: a: (aOnly & both & bOnly):
        Dict.insert key a aOnly & both & bOnly

    onBOnly =
        key: b: (aOnly & both & bOnly):
        aOnly & both & Dict.insert key b bOnly

    onBoth =
        key: a: b: (aOnly & both & bOnly):
        aOnly & Dict.insert key (a & b) both & bOnly

    Dict.merge onAOnly onBoth onBOnly da db (Dict.empty & Dict.empty & Dict.empty)


recordIsCompatibleWith as Env: Pos: Dict Name Type: Type: Monad None =
    env: pos: expectedAttrs: actualType:

    log "EXP" { exp = typeToText env (CA.TypeRecord Pos.G expectedAttrs), act = typeToText env actualType }

    try actualType as
        CA.TypeRecord _ aAttrs:
            eOnly & both & aOnly =
                onlyBothOnly expectedAttrs aAttrs

            if eOnly /= Dict.empty then
                addCheckError pos [
                    , "missing attributes: " .. toHuman (Dict.keys eOnly)
                    ]
            else if aOnly /= Dict.empty then
                addCheckError pos [
                    , "extra attributes: " .. toHuman (Dict.keys aOnly)
                    ]
            else
                None >> dict_for both attrName: (eType & aType): None:
                    isCompatibleWith env eType pos aType

        CA.TypeRecordExt _ name flags aAttrs:
            eOnly & both & aOnly =
                onlyBothOnly expectedAttrs aAttrs

            if aOnly == Dict.empty then
                return None
            else
                addCheckError pos [
                    , "extra attributes: " .. toHuman (Dict.keys aOnly)
                    ]

            if flags.allowFunctions then
                return None
            else
                todo "ensure that there are no functions in expectedAttrs"



            None >> dict_for both attrName: (eType & aType): None:
                isCompatibleWith env eType pos aType

        _:
            addCheckError pos [
                , "I was expecting a record like:"
                , typeToText env (CA.TypeRecord Pos.G expectedAttrs)
                , "but the actual type is: "
                , typeToText env actualType
                , "The two types are not compatible!"
                ]






onlyBothUnifiedOnly as Env: UnifyReason: Pos: Dict Text Type: Dict Text Type: Monad (Dict Text Type & Dict Text Type & Dict Text Type) =
    env: reason: pos: a_attrs: b_attrs:

    aOnly & both & bOnly =
        onlyBothOnly a_attrs b_attrs

    dict_map (k: ( a & b ): unify_ env reason pos a b) both >> andThen bothUnified:

    return << aOnly & bothUnified & bOnly





unifyRecords as Env: UnifyReason: Pos: Dict Text Type: Type: Monad Type =
    env: reason: pos: a_attrs: t2:

    try t2 as
        CA.TypeRecord _ b_attrs:

            onlyBothUnifiedOnly env reason pos a_attrs b_attrs >> andThen (aOnly & bothUnified & bOnly):

            if bOnly == Dict.empty and aOnly == Dict.empty then
                # the two are the same
                return (CA.TypeRecord (Pos.I 4) bothUnified)

            else
                e =
                    IncompatibleRecords {
                        , bOnly = Dict.keys bOnly
                        , aOnly = Dict.keys aOnly
                        , bothUnified = Dict.keys bothUnified
                        }

                unifyError pos e (CA.TypeRecord pos a_attrs) (CA.TypeRecord pos b_attrs)

        CA.TypeRecordExt _ bName flags b_attrs:
            onlyBothUnifiedOnly env reason pos a_attrs b_attrs >> andThen (aOnly & bothUnified & bOnly):
            unifyToNonExtensibleRecord env pos reason bName bOnly aOnly bothUnified

        _:
            addError pos [
                , "A type is a record like:"
                , typeToText env (CA.TypeRecord Pos.G a_attrs)
                , "but the actual type is: "
                , typeToText env t2
                , "The two types are not compatible!"
                ]


unifyRecordsExt as Env: UnifyReason: Pos: Text: CA.TyvarFlags: Dict Text Type: Type: Monad Type =
    env: reason: pos: aName: aFlags: aAttrs: b:

    try b as
        CA.TypeRecord _ bAttrs:
            onlyBothUnifiedOnly env reason pos aAttrs bAttrs >> andThen (aOnly & bothUnified & bOnly):
            unifyToNonExtensibleRecord env pos reason aName aOnly bOnly bothUnified

        CA.TypeRecordExt _ bName bFlags bAttrs:
            onlyBothUnifiedOnly env reason pos aAttrs bAttrs >> andThen (aOnly & bothUnified & bOnly):

            if aName == bName and aOnly == Dict.empty and bOnly == Dict.empty then

                todo "check flags!!!"

                return << CA.TypeRecordExt pos aName aFlags bothUnified

            else
                newName identity >> andThen new:

                todo "merge flags!"

                sub =
                    CA.TypeRecordExt pos new aFlags (Dict.join bOnly bAttrs)

                addSubstitution env "jj1" pos reason aName sub >> andThen _:
                addSubstitution env "jj2" pos reason bName sub >> andThen _:
                return sub

        _:
            addError pos [
                , "I was expecting a record like:"
                , typeToText env (CA.TypeRecordExt Pos.G aName aFlags aAttrs)
                , "but the actual type is: "
                , typeToText env b
                , "The two types are not compatible!"
                ]


unifyToNonExtensibleRecord as Env: Pos: UnifyReason: Name: Dict Name Type: Dict Name Type: Dict Name Type: Monad Type =
    env: pos: reason: aName: aOnly: bOnly: bothUnified:
    if aOnly /= Dict.empty then
        # b is missing attributes but is not extensible
        addError pos
            [ "record is missing attrs: " .. (aOnly >> Dict.keys >> Text.join ", ")
            , toHuman reason
            ]

    else
        # the `a` tyvar should contain the missing attributes, ie `bOnly`
        newName identity >> andThen ext:
        flags = todo "lnasdpuigsdflkj"
        addSubstitution env "ne" pos reason aName (CA.TypeRecordExt (Pos.I 5) ext flags bOnly) >> andThen _:
        Dict.join bothUnified bOnly
            >> CA.TypeRecord pos
            >> return


