module Compiler.TypeCheck exposing (..)

{-|

This module implements what is think is the (Damas)-Hindley-Milner inference algorithm, but I'm not sure.


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

        1.  If the return value contains a function like `(a -> a)`, but functions can't be returned as we said above.

        2.  Functions that never actually return, ie `Debug.crash as Text -> a`

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

-}

import Compiler.CoreModule as Core
import Dict exposing (Dict)
import Human.CanonicalAst as HCA
import RefHierarchy
import Set exposing (Set)
import StateMonad as M exposing (M, do, return)
import Types.CanonicalAst as CA exposing (Pos, Type)
import Types.Error as Error exposing (Error, Res)
import Types.Literal


{-| TODO do we define this somewhere else?
-}
type alias Name =
    String



----
--- Env
--
-- TODO rename to Scope?


type alias Env =
    { instanceVariables : Dict Name InstanceVariable
    , constrainedTypeVariables : Dict Name TypeVariable
    }


initEnv : Env
initEnv =
    { instanceVariables = Dict.empty
    , constrainedTypeVariables = Dict.empty
    }


type alias InstanceVariable =
    { definedAt : CA.Pos
    , ty : Type
    , freeTypeVariables : Dict Name TypeVariable
    , isMutable : Bool
    }


type alias TypeVariable =
    { definedAt : CA.Pos
    , rf : List CA.RejectFunction
    }



----
--- State
--


type alias State =
    { nextName : Int
    , errors : List Error

    -- TODO not sure they should be here, substitutions might make sense only within a specific environment
    , substitutions : Subs

    -- This is used only by the unify function, which resets it every time
    --
    -- When an unification fails, a new placeholder type variable is returned, and the two clashing types are recorder here
    --
    , typeClashesByPlaceholderId : Dict Name TypeClash
    }


initState : State
initState =
    { nextName = 0
    , errors = []
    , substitutions = Dict.empty
    , typeClashesByPlaceholderId = Dict.empty
    }


type alias TypeClash =
    { t1 : Type
    , t2 : Type
    , err : UnifyError
    }


type alias Subs =
    Dict Name Type



----
--- State monad
--


type alias Monad a =
    M State a


newName : (Name -> a) -> Monad a
newName f state =
    ( f (String.fromInt state.nextName)
    , { state | nextName = state.nextName + 1 }
    )


newType : Pos -> List CA.RejectFunction -> Monad Type
newType pos rf =
    newName (CA.TypeVariable pos rf)


insertError : Error -> Monad ()
insertError e state =
    ( ()
    , { state | errors = e :: state.errors }
    )


getSubstitutions : Monad Subs
getSubstitutions state =
    ( state.substitutions
    , state
    )


insertTypeClash : Name -> Type -> Type -> UnifyError -> Monad ()
insertTypeClash id t1 t2 err state =
    ( ()
    , { state
        | typeClashesByPlaceholderId =
            Dict.insert id
                { t1 = t1
                , t2 = t2
                , err = err
                }
                state.typeClashesByPlaceholderId
      }
    )


popClashingtypes : Monad (Dict Name TypeClash)
popClashingtypes state =
    -- TODO avoid reallocation when state.typeClashesByPlaceholderId == Dict.empty?
    ( state.typeClashesByPlaceholderId
    , { state | typeClashesByPlaceholderId = Dict.empty }
    )



----
--- Inference
--


fromAllValueDefs : Env -> List CA.RootValueDef -> Res Env
fromAllValueDefs env valueDefs =
    let
        ( annotated, nonAnnotated ) =
            List.partition (\d -> d.maybeAnnotation /= Nothing) valueDefs
    in
    case RefHierarchy.reorder .name getValueRefs nonAnnotated of
        Err circulars ->
            -- TODO test this error. Is it "circular" or "recursive"?
            "These definitions are circular but don't have a type annotation: "
                ++ String.join ", " (circulars |> Set.fromList |> Set.toList)
                |> Error.errorTodo

        Ok orderedNonAnnotated ->
            let
                allOrdered =
                    orderedNonAnnotated ++ annotated

                ( envF, stateF ) =
                    annotated
                        |> List.foldl insertAnnotatedRootValue env
                        |> M.list_foldl fromRootDefinition allOrdered
                        |> M.run initState
            in
            if stateF.errors == [] then
                Ok envF

            else
                stateF.errors
                    |> Error.Nested
                    |> Err


fromRootDefinition : CA.RootValueDef -> Env -> Monad Env
fromRootDefinition def env =
    case ( def.maybeAnnotation, def.isNative ) of
        ( Just annotation, True ) ->
            return { env | instanceVariables = Dict.insert def.name (makeNative annotation def) env.instanceVariables }

        _ ->
            fromDefinition env False (CA.PatternAny def.pos def.name) def.maybeAnnotation def.body


makeNative : Type -> CA.RootValueDef -> InstanceVariable
makeNative ty def =
    { definedAt = def.pos
    , ty = ty
    , freeTypeVariables = getFreeTypeVars Dict.empty ty
    , isMutable = False
    }


{-| TODO It might be faster if we write down the references while we build the canonical AST
TODO It might be _even faster_ if we use the references to decide what to compile and what not to.
-}
getValueRefs : CA.RootValueDef -> Set String
getValueRefs def =
    let
        fn : CA.PosMap -> a -> M (Set String) a
        fn fold ext =
            case fold of
                CA.PosMap_Expr (CA.Variable _ args) ->
                    if args.isRoot then
                        do (M.update (Set.insert args.name)) <| \_ ->
                        return ext

                    else
                        return ext

                _ ->
                    return ext
    in
    Set.empty
        |> CA.posMap_rootValueDef fn def
        |> Tuple.second


insertAnnotatedRootValue : CA.RootValueDef -> Env -> Env
insertAnnotatedRootValue def env =
    case def.maybeAnnotation of
        Nothing ->
            env

        Just ty ->
            { env
                | instanceVariables =
                    Dict.insert def.name
                        { definedAt = def.pos
                        , ty = ty
                        , freeTypeVariables = getFreeTypeVars Dict.empty ty
                        , isMutable = False
                        }
                        env.instanceVariables
            }


fromBlock : Env -> List CA.Statement -> Monad Type
fromBlock env0 block =
    let
        -- this is another state, not the one defined above
        state0 =
            { env = env0
            , mutableDefs = []
            , inferredType = Core.noneType
            }

        upd statement state =
            do (fromStatement state.env statement) <| \( env, maybeMutableDefinitionId, inferredType ) ->
            return
                { env = env
                , inferredType = inferredType
                , mutableDefs =
                    case maybeMutableDefinitionId of
                        Nothing ->
                            state.mutableDefs

                        Just definitionId ->
                            definitionId :: state.mutableDefs
                }
    in
    do (M.list_foldl upd block state0) <| \stateF ->
    case stateF.mutableDefs of
        [] ->
            return stateF.inferredType

        head :: tail ->
            -- A block is actually allowed to return tyvars that allow functions
            if typeContainsFunctions { checkTyvars = False } stateF.inferredType then
                errorTodo head "blocks that define mutables can't return functions"

            else
                return stateF.inferredType


fromStatement : Env -> CA.Statement -> Monad ( Env, Maybe CA.Pos, Type )
fromStatement env statement =
    case statement of
        CA.Evaluation expr ->
            do (fromExpression env expr) <| \expressionType ->
            return ( env, Nothing, expressionType )

        CA.Definition def ->
            do (fromDefinition env def.mutable def.pattern def.maybeAnnotation def.body) <| \env1 ->
            return
                ( env1
                , if def.mutable then
                    Just <| CA.patternPos def.pattern

                  else
                    Nothing
                , Core.noneType
                )


applySubsToType : Type -> Monad Type
applySubsToType ty =
    do getSubstitutions <| \subs ->
    return <| replaceTypeVariables subs ty


fromDefinition : Env -> Bool -> CA.Pattern -> Maybe Type -> List CA.Statement -> Monad Env
fromDefinition env isMutable pattern maybeAnnotation body =
    case maybeAnnotation of
        Nothing ->
            -- No annotation: recursive definitions are not allowed
            -- This means that *first* we infer the definition, *then* we add it to the environment
            --
            -- TODO if the user tries to use the variable recursively, tell them they need an annotation
            --
            -- TODO ability to infer recursive definitions, but only if I don't have to rewrite everything
            --
            do (fromBlock env body) <| \bodyType_ ->
            do (applySubsToType bodyType_) <| \bodyType ->
            do (fromPattern env pattern Dict.empty) <| \patternOut ->
            do (unify patternOut.pos UnifyReason_DefBlockVsPattern bodyType patternOut.ty) <| \unifiedType ->
            do (applySubsToPatternVarsAndAddThemToEnv isMutable patternOut.vars env) <| return

        Just annotatedType ->
            -- There is an annotation: allow recursive definitions
            -- This means that we can add it to the environment before we actually infer it
            do (fromPattern env pattern Dict.empty) <| \patternOut ->
            do (unify patternOut.pos UnifyReason_AnnotationVsPattern annotatedType patternOut.ty) <| \_ ->
            -- applySubsToPatternVarsAndAddThemToEnv also adds type variables, which is why we're using it here!
            -- TODO maybe I should make it more explicit? Use an ad-hoc function?
            do (applySubsToPatternVarsAndAddThemToEnv isMutable patternOut.vars env) <| \env1 ->
            do (fromBlock env1 body) <| \bodyType ->
            do (unify patternOut.pos UnifyReason_AnnotationVsBlock annotatedType bodyType) <| \unifiedBlockType ->
            do (checkFreeVariables patternOut.pos annotatedType bodyType) <| \() ->
            return env1


checkFreeVariables : Pos -> Type -> Type -> Monad ()
checkFreeVariables pos annotatedType blockType =
    let
        annotatedFreeVars =
            typeTyvars annotatedType

        actualFreeVars =
            typeTyvars blockType
    in
    if Dict.size annotatedFreeVars > Dict.size actualFreeVars then
        do
            (monadError pos
                [ Error.text "The annotation is too general"
                , Error.text ""
                , Error.text <| "The annotation uses: " ++ String.join ", " (Dict.keys annotatedFreeVars)
                , error_type annotatedType
                , Error.text ""
                , Error.text <| "But the actual type uses only: " ++ String.join ", " (Dict.keys actualFreeVars)
                , error_type blockType
                , Error.text ""
                , Error.text <| "The annotation has " ++ String.fromInt (Dict.size annotatedFreeVars - Dict.size actualFreeVars) ++ " type variables too many"
                ]
            )
        <| \_ ->
        return ()

    else
        return ()


{-| The general idea, and I don't know if it actually makes sense, is that we want a function that

1.  for a given expression tries to figure out its type
2.  collects type errors within the expression itself

-}
fromExpression : Env -> CA.Expression -> Monad Type
fromExpression env expression =
    case expression of
        CA.Literal pos l ->
            fromLiteral l
                |> return

        CA.Variable pos { name, attrPath } ->
            case Dict.get name env.instanceVariables of
                Nothing ->
                    errorUndefinedVariable pos name

                Just var ->
                    {-

                       The basic problem
                       -------------------

                       Consider:
                       ```
                       emptyList : [ item ]
                       emptyList = []

                       listOfText : [ Text ]
                       listOfText = "blah" :: emptyList

                       listOfBool : [ Bool ]
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
                    -}
                    do (replaceTypeVariablesWithNew var) <| \varType ->
                    {-

                       Dealing with attributes as a special case (rather than just as an access function)
                       allows Squarepants to mutate single attributes of a mutable record:

                       @someRecord.someAttribute += 1

                       @someRecord.someOtherAttribute.someNestedAttribute := "Squidward"

                    -}
                    applyAttributePath pos attrPath varType

        CA.Lambda pos param body ->
            do (fromParameter env param) <| \( isMutable, patternOut ) ->
            let
                bodyEnv =
                    insertPatternVars Dict.empty True isMutable patternOut.vars env
            in
            do (fromBlock bodyEnv body) <| \bodyType ->
            -- fromParameter can infer paramType only when destructuring some patterns, it's not reliable in general
            -- fromBlock instead infers paramType fully, but will not apply the substitutions it creates
            -- So we just pull out the substitutions and apply them to paramType
            do getSubstitutions <| \subs ->
            CA.TypeFunction pos (replaceTypeVariables subs patternOut.ty) isMutable bodyType
                |> return

        CA.Call pos reference argument ->
            -- first of all, let's get our children types
            do (fromExpression env reference) <| \referenceType ->
            do (fromArgument env argument) <| \( fromIsMutable, argumentType ) ->
            -- the type of the call itself is the return type of the lamba
            unifyFunctionOnCallAndYieldReturnType env pos referenceType fromIsMutable argumentType

        CA.If pos ar ->
            do (fromBlock env ar.condition) <| \conditionType ->
            do (unify pos UnifyReason_IfCondition conditionType Core.boolType) <| \_ ->
            do getSubstitutions <| \s ->
            do (fromBlock env ar.true) <| \trueType ->
            do (fromBlock env ar.false) <| \falseType ->
            unify pos UnifyReason_IfBranches trueType falseType

        CA.Try pos value patternsAndBlocks ->
            do (fromExpression env value) <| \tryType ->
            do (newType pos []) <| \newBlockType ->
            do (M.list_foldl (fromPatternAndBlock env) patternsAndBlocks ( tryType, newBlockType )) <| \( patternType, inferredBlockType ) ->
            return inferredBlockType

        CA.Record pos maybeExt attrValues ->
            do (M.dict_map (\k -> fromExpression env) attrValues) <| \attrTypes ->
            case maybeExt of
                Nothing ->
                    return <| CA.TypeRecord pos Nothing attrTypes

                Just variableArgs ->
                    -- TODO here it would be easier to support an entire expression
                    do (fromExpression env (CA.Variable pos variableArgs)) <| \ty_ ->
                    do (applySubsToType ty_) <| \ty ->
                    do (newName identity) <| \name ->
                    do (unify pos (UnifyReason_AttributeUpdate (Dict.keys attrTypes)) ty (CA.TypeRecord pos (Just name) attrTypes)) <| \unifiedType ->
                    return unifiedType


unifyFunctionOnCallAndYieldReturnType : Env -> CA.Pos -> Type -> Bool -> Type -> Monad Type
unifyFunctionOnCallAndYieldReturnType env pos callReferenceType callIsMutable callArgumentType =
    case callReferenceType of
        CA.TypeFunction _ refArgumentType refIsMutable refReturnType ->
            if callIsMutable /= refIsMutable then
                errorTodo pos "mutability clash 2"

            else
                do (unify pos UnifyReason_CallArgument callArgumentType refArgumentType) <| \unifiedArgumentType ->
                do getSubstitutions <| \subs ->
                return <| replaceTypeVariables subs refReturnType

        CA.TypeVariable pos_ rf name ->
            {-
               I don't think there's any way this could happen normally:
               * if it's a variable, we should have a function type already in the env
               * if it's a lambda, we should have inferred it
               * it could be a recursive function, but with a recursive function we infer its type anyway as a function...?

               Regardless, it definitely can happen if an undefined variable is being used as a function, and it must be
               dealt with to reduce error messages noise.
            -}
            if rf /= [] then
                errorTodo pos (rf |> List.map Debug.toString |> String.join ", ")

            else
                do (newType pos []) <| \returnType ->
                let
                    ty =
                        CA.TypeFunction pos callArgumentType callIsMutable returnType
                in
                do (unify pos UnifyReason_IsBeingCalledAsAFunction callReferenceType ty) <| \_ ->
                do getSubstitutions <| \subs ->
                return <| replaceTypeVariables subs returnType

        _ ->
            unifyError NotAFunction callReferenceType callReferenceType


fromLiteral : Types.Literal.Value -> Type
fromLiteral l =
    case l of
        Types.Literal.Number _ ->
            Core.numberType

        Types.Literal.Text _ ->
            Core.textType

        Types.Literal.Char _ ->
            Core.charType


fromPatternAndBlock : Env -> ( CA.Pattern, List CA.Statement ) -> ( Type, Type ) -> Monad ( Type, Type )
fromPatternAndBlock env ( pattern, block ) ( patternTypeSoFar, blockTypeSoFar ) =
    do (fromPattern env pattern Dict.empty) <| \patternOut ->
    do (unify patternOut.pos UnifyReason_TryPattern patternOut.ty patternTypeSoFar) <| \unifiedPatternType ->
    -- TODO do I really need to apply subs here?
    do (applySubsToPatternVarsAndAddThemToEnv False patternOut.vars env) <| \patternEnv ->
    do (fromBlock patternEnv block) <| \blockType ->
    -- TODO pos should be the block's last statement
    do (unify patternOut.pos UnifyReason_TryBlock blockType blockTypeSoFar) <| \unifiedBlockType ->
    return ( unifiedPatternType, unifiedBlockType )


fromArgument : Env -> CA.Argument -> Monad ( Bool, Type )
fromArgument env argument =
    case argument of
        CA.ArgumentExpression expr ->
            do (fromExpression env expr) <| \ty ->
            return ( False, ty )

        CA.ArgumentMutable pos { name, attrPath } ->
            case Dict.get name env.instanceVariables of
                Nothing ->
                    do (errorUndefinedVariable pos name) <| \ty ->
                    return ( True, ty )

                Just var ->
                    if not var.isMutable then
                        do (errorTryingToMutateAnImmutable pos name) <| \ty ->
                        return ( True, ty )

                    else if typeContainsFunctions { checkTyvars = True } var.ty then
                        do (errorTodo pos "mutable arguments can't allow functions") <| \ty ->
                        return ( True, ty )

                    else
                        do (applyAttributePath pos attrPath var.ty) <| \ty ->
                        return ( True, ty )


errorTryingToMutateAnImmutable : Pos -> Name -> Monad Type
errorTryingToMutateAnImmutable pos name =
    monadError pos
        [ Error.text <| "You are trying to mutate variable `" ++ name ++ "` but it was declared as not mutable!"
        , Error.text ""
        , Error.text "TODO [link to wiki page that explains how to declare variables]"
        ]


fromParameter : Env -> CA.Parameter -> Monad ( Bool, PatternOut )
fromParameter env param =
    case param of
        CA.ParameterPattern pattern ->
            do (fromPattern env pattern Dict.empty) <| \patternOut ->
            return ( False, patternOut )

        CA.ParameterMutable pos paramName ->
            -- TypeNonFunction
            do (newType pos [ CA.Pa pos ]) <| \ty ->
            return
                ( True
                , { vars = Dict.singleton paramName ( pos, ty )
                  , pos = pos
                  , ty = ty
                  }
                )


{-| Patterns are special because they are the one way to **add variables to the env**.

When we have information about the type of these variables we want to add

                (because they are used in a block, for example)

Usually these variables are used in a block, and ide

Often we have important information about these variables, be

If we have information about the type of these variables ( because they are used in a block)

              There are two ways new variables can be added to the env: via definitions ("x = ...") or as function arguments ("fn x: ...")

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
  (x, y) =
      (1, 2)

  pattern -> (a, b)
  block -> (Int, Int)

  devo unificarlo e poi applicare le subs all'env?

  Do I need to apply substitutions to the env?
```

  - fromParameter
    fn var:
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

-}
type alias PatternVars =
    Dict Name ( Pos, Type )


type alias PatternOut =
    { vars : PatternVars
    , pos : Pos
    , ty : Type
    }


fromPattern : Env -> CA.Pattern -> PatternVars -> Monad PatternOut
fromPattern env pattern vars =
    case pattern of
        CA.PatternDiscard pos ->
            do (newType pos []) <| \ty ->
            return <| PatternOut vars pos ty

        CA.PatternAny pos name ->
            do (newType pos []) <| \ty ->
            return <| PatternOut (Dict.insert name ( pos, ty ) vars) pos ty

        CA.PatternLiteral pos literal ->
            return <| PatternOut vars pos (fromLiteral literal)

        CA.PatternConstructor pos ref args ->
            case Dict.get ref env.instanceVariables of
                Nothing ->
                    -- TODO still add all variables defined in the args, otherwise there will
                    -- missing variables that will trigger misleading "undefined variable" errors
                    -- (ie, use unifyConstructorWithItsArgs anyway)
                    do (errorUndefinedVariable pos ref) <| \ety ->
                    return <| PatternOut vars pos ety

                Just instanceVar ->
                    do (replaceTypeVariablesWithNew instanceVar) <| \constructorTy ->
                    let
                        p : UnifyConstructorWithItsArgsParams
                        p =
                            { env = env
                            , ref = ref
                            , pos = pos
                            , ty = constructorTy
                            , args = args
                            , argIndex = 0
                            , vars = vars
                            }
                    in
                    do (unifyConstructorWithItsArgs p) <| \( patternVars, patternTy ) ->
                    return <| PatternOut patternVars pos patternTy

        CA.PatternRecord pos attrs ->
            let
                blah name pa ( varsX, attrTypes ) =
                    do (fromPattern env pa varsX) <| \paOut ->
                    return ( paOut.vars, Dict.insert name paOut.ty attrTypes )
            in
            do (M.dict_foldl blah attrs ( vars, Dict.empty )) <| \( vars1, attrTypes ) ->
            do (newName identity) <| \extName ->
            return <| PatternOut vars1 pos (CA.TypeRecord pos (Just extName) attrTypes)


type alias UnifyConstructorWithItsArgsParams =
    { env : Env
    , ref : Name
    , pos : Pos
    , ty : Type
    , args : List CA.Pattern
    , argIndex : Int
    , vars : PatternVars
    }


unifyConstructorWithItsArgs : UnifyConstructorWithItsArgsParams -> Monad ( PatternVars, Type )
unifyConstructorWithItsArgs p =
    case ( p.ty, p.args ) of
        -- Argument needed, argument given
        ( CA.TypeFunction _ from _ to, head :: tail ) ->
            do (fromPattern p.env head p.vars) <| \{ vars, pos, ty } ->
            do (unify pos (UnifyReason_ConstructorArgument p) from ty) <| \unifiedFrom ->
            unifyConstructorWithItsArgs
                { p
                    | argIndex = p.argIndex + 1
                    , ty = to
                    , args = tail
                    , vars = vars
                }

        -- Error: Argument needed but not given!
        ( CA.TypeFunction _ from _ to, [] ) ->
            -- TODO tell how many are needed and how many are actually given
            do (errorTodo p.pos <| "Type constructor " ++ p.ref ++ " is missing an argument " ++ String.fromInt p.argIndex) <| \ety ->
            return ( p.vars, ety )

        -- No arguments needed, no arguments given
        ( _, [] ) ->
            do getSubstitutions <| \subs ->
            -- TODO should I apply subs here?
            return ( p.vars, p.ty )

        -- Error: no argument needed, but argument given!
        ( _, head :: tail ) ->
            -- TODO tell how many are needed and how many are actually given
            do (errorTodo p.pos <| "Type constructor " ++ p.ref ++ " has too many args") <| \ety ->
            return ( p.vars, ety )



----
--- Unification
--


type UnifyReason
    = UnifyReason_AnnotationVsPattern
    | UnifyReason_AnnotationVsBlock
    | UnifyReason_DefBlockVsPattern
    | UnifyReason_CallArgument
    | UnifyReason_IsBeingCalledAsAFunction
    | UnifyReason_IfCondition
    | UnifyReason_IfBranches
    | UnifyReason_TryPattern
    | UnifyReason_TryBlock
    | UnifyReason_ConstructorArgument UnifyConstructorWithItsArgsParams
    | UnifyReason_AttributeAccess Name
    | UnifyReason_AttributeUpdate (List Name)


type UnifyError
    = IncompatibleTypes Name Name
    | IncompatibleMutability
    | IncompatibleRecords
    | Cycle Name
    | NotAFunction
    | NonFunctionContainsFunction (List CA.RejectFunction)
    | OkThisIsActuallyPossible
    | NI String


unify : CA.Pos -> UnifyReason -> Type -> Type -> Monad Type
unify pos reason a b =
    do (unify_ reason pos [] a b) <| \unifiedType ->
    do popClashingtypes <| \typeClashes ->
    if typeClashes == Dict.empty then
        return unifiedType

    else
        -- there were type clashes, so turn them into errors
        do (errorIncompatibleTypes reason pos unifiedType typeClashes) <| \_ ->
        return unifiedType


{-| Unification is always successful: if two types can't be unified, then the unification error is added to the state and a brand new type variable is returned in place of the clashing types.

NOTE: the t1 should be the type with an actual position in the code because its position is the one that will be used.

TODO: when do I refine the environment?
  ----> never, ideally: if it's a definition we know its value after inferring its body, if it's a lambda parameter we know it when we stop using it.
  Keeping the subs up to date should be enough

Are substitutions interesting only against a specific environment instance?

-}
unify_ : UnifyReason -> Pos -> List CA.RejectFunction -> Type -> Type -> Monad Type
unify_ reason pos1 rf t1 t2 =
    {- TODO
       if constructors didn't have pos, we could test `t1 == t2` and exit early

        This is the one time I regret having pos as a constructor param instead than as a wrapper.
    -}
    case ( t1, t2 ) of
        ( CA.TypeAlias pos _ aliased, _ ) ->
            unify_ reason pos rf aliased t2

        ( _, CA.TypeAlias _ _ aliased ) ->
            unify_ reason pos1 rf t1 aliased

        ( CA.TypeConstant pos ref1 args1, CA.TypeConstant _ ref2 args2 ) ->
            if ref1 /= ref2 then
                unifyError (IncompatibleTypes ref1 ref2) t1 t2

            else
                let
                    fold arg1 arg2 =
                        unify_ reason pos rf arg1 arg2
                in
                -- TODO should I check arity here?
                do (M.list_map2 (unify_ reason pos rf) args1 args2) <| \argTypes ->
                do getSubstitutions <| \subs ->
                argTypes
                    |> List.map (replaceTypeVariables subs)
                    |> CA.TypeConstant pos ref1
                    |> return

        ( CA.TypeVariable pos rf1 v1_name, CA.TypeVariable _ rf2 v2_name ) ->
            if v1_name == v2_name then
                return t1

            else
                do getSubstitutions <| \subs ->
                case ( Dict.get v1_name subs, Dict.get v2_name subs ) of
                    ( Just sub1, Just sub2 ) ->
                        let
                            rfn =
                                rf ++ rf1 ++ rf2
                        in
                        do (unify_ reason pos1 rfn sub1 sub2) <| \v ->
                        do (addSubstitution { overrideIsAnError = False } reason v1_name rfn v) <| \_ ->
                        do (addSubstitution { overrideIsAnError = False } reason v2_name rfn v) <| \subbedTy ->
                        return subbedTy

                    ( Nothing, Just sub2 ) ->
                        addSubstitution { overrideIsAnError = True } reason v1_name (rf ++ rf1) t2

                    _ ->
                        addSubstitution { overrideIsAnError = True } reason v2_name (rf ++ rf2) t1

        ( CA.TypeVariable _ rf1 name1, _ ) ->
            addSubstitution { overrideIsAnError = True } reason name1 rf1 t2

        ( _, CA.TypeVariable _ rf2 name2 ) ->
            addSubstitution { overrideIsAnError = True } reason name2 rf2 t1

        ( CA.TypeFunction pos a_from a_fromIsMutable a_to, CA.TypeFunction _ b_from b_fromIsMutable b_to ) ->
            if rf /= [] then
                unifyError (NonFunctionContainsFunction rf) t1 t2

            else if a_fromIsMutable /= b_fromIsMutable then
                unifyError IncompatibleMutability t1 t2

            else
                {- TODO
                     should this be checked here? This is not really an **unification** error

                     It doesn't look like this problem can arise as result of a unification. o_O

                   let
                       newNf =
                           if a_fromIsMutable then
                               "mutable args can't be or contain functions" :: rf

                           else
                               rf
                   in
                -}
                do (unify_ reason pos rf a_from b_from) <| \unified_from ->
                do getSubstitutions <| \subs_ ->
                {- Without the replacement here, a function annotation will produce circular substitutions

                   id a =
                       as a -> a
                       a

                -}
                do (unify_ reason pos rf (replaceTypeVariables subs_ a_to) (replaceTypeVariables subs_ b_to)) <| \unified_to ->
                do getSubstitutions <| \subs ->
                CA.TypeFunction pos (replaceTypeVariables subs unified_from) a_fromIsMutable (replaceTypeVariables subs unified_to)
                    |> return

        ( CA.TypeRecord pos a_ext a_attrs, CA.TypeRecord _ b_ext b_attrs ) ->
            unifyRecords reason pos rf ( a_ext, a_attrs ) ( b_ext, b_attrs )

        _ ->
            monadError pos1
                [ Error.text "unify_"
                , Error.text "t1:"
                , error_type t1
                , Error.text "t2:"
                , error_type t2
                , Error.text <| Debug.toString reason
                ]


addSubstitution : { overrideIsAnError : Bool } -> UnifyReason -> Name -> List CA.RejectFunction -> Type -> Monad Type
addSubstitution { overrideIsAnError } reason name rf ty =
    --     let
    --         _ =
    --             Debug.log "addSubstitution" { reason = reason, name = name, ty = ty }
    --     in
    do getSubstitutions <| \subs ->
    -- TODO no need to do a dict.get if not overrideIsAnError
    case ( overrideIsAnError, Dict.get name subs ) of
        ( True, Just sub ) ->
            monadError (CA.I 3)
                [ Error.text <| "Compiler bug: Substitution for tyvar `" ++ name ++ "` is being overwritten."
                , Error.text <| "Old type " ++ HCA.typeToString ty
                , Error.text <| "New type " ++ HCA.typeToString sub
                , Error.text <| "This is not your fault, it's a bug in the Squarepants compiler."
                , Error.text <| Debug.toString reason
                ]

        _ ->
            let
                newTyContainsSubbedTyvars =
                    List.any (\tyvarName -> typeHasTyvar tyvarName ty) (Dict.keys subs)

                newNameIsUsedInSubbingTypes =
                    List.any (typeHasTyvar name) (Dict.values subs)
            in
            if typeHasTyvar name ty then
                monadError (CA.I 14)
                    [ Error.text <| "Compiler bug: Trying to add a cyclical substitution for tyvar `" ++ name ++ "`: " ++ HCA.typeToString ty
                    , Error.text "This is not your fault, it's a bug in the Squarepants compiler."
                    ]
                {- TODO re-think and properly implement NonFunction check and its propagation to non-constrained tyvars.

                   else if rf /= [] && typeContainsFunctions { checkTyvars = True } ty then
                       -- TODO this one unification error does not need two types, but a type and a name?
                       unifyError (NonFunctionContainsFunction rf) (CA.TypeVariable (CA.I 12) rf name) ty
                -}

            else if newTyContainsSubbedTyvars && newNameIsUsedInSubbingTypes then
                monadError (CA.I 24)
                    [ Error.text "Compiler bug: mutually circular substitution"
                    , Error.text <| "On tyvar: `" ++ name ++ "`, with subbing type: " ++ HCA.typeToString ty
                    , Error.text "This is not your fault, it's a bug in the Squarepants compiler."
                    , Error.text ""
                    , Error.text "circular subs:"
                    , subs
                        |> Dict.filter (\k -> typeHasTyvar name)
                        |> Debug.toString
                        |> Error.text
                    , Error.text ""
                    , Error.text <| "name: " ++ name
                    , Error.text <| "ty: " ++ HCA.typeToString ty
                    ]

            else
                let
                    ( updatedSubs, updatedNewType ) =
                        if newTyContainsSubbedTyvars then
                            let
                                -- apply old subs to ty
                                t =
                                    replaceTypeVariables subs ty
                            in
                            ( Dict.insert name t subs
                            , t
                            )

                        else if newNameIsUsedInSubbingTypes then
                            -- apply new substitution to all old substitutions
                            ( subs
                                |> Dict.map (\k -> replaceTypeVariables (Dict.singleton name ty))
                                |> Dict.insert name ty
                            , ty
                            )

                        else
                            ( Dict.insert name ty subs
                            , ty
                            )
                in
                \state ->
                    ( updatedNewType
                    , { state | substitutions = updatedSubs }
                    )


typeContainsFunctions : { checkTyvars : Bool } -> Type -> Bool
typeContainsFunctions p ty =
    case ty of
        CA.TypeConstant _ _ _ ->
            False

        CA.TypeVariable _ rf _ ->
            p.checkTyvars && rf == []

        CA.TypeFunction _ from fromIsMutable to ->
            True

        CA.TypeAlias _ path t ->
            typeContainsFunctions p t

        CA.TypeRecord _ extensible attrs ->
            attrs
                |> Dict.values
                |> List.any (typeContainsFunctions p)


typeHasTyvar : Name -> Type -> Bool
typeHasTyvar n ty =
    case ty of
        CA.TypeVariable pos rf name ->
            n == name

        CA.TypeFunction _ from fromIsMutable to ->
            typeHasTyvar n from || typeHasTyvar n to

        CA.TypeConstant pos ref args ->
            List.any (typeHasTyvar n) args

        CA.TypeAlias _ path t ->
            typeHasTyvar n t

        CA.TypeRecord pos extensible attrs ->
            Just n == extensible || List.any (typeHasTyvar n) (Dict.values attrs)


typeTyvars : Type -> Dict Name TypeVariable
typeTyvars ty =
    case ty of
        CA.TypeVariable pos rf name ->
            -- TODO is pos equivalent to definedAt?
            Dict.singleton name { definedAt = pos, rf = rf }

        CA.TypeFunction _ from fromIsMutable to ->
            Dict.union (typeTyvars from) (typeTyvars to)

        CA.TypeConstant pos ref args ->
            List.foldl (\a -> Dict.union (typeTyvars a)) Dict.empty args

        CA.TypeAlias _ path t ->
            typeTyvars t

        CA.TypeRecord pos extensible attrs ->
            let
                init =
                    case extensible of
                        Nothing ->
                            Dict.empty

                        Just name ->
                            Dict.singleton name { definedAt = pos, rf = [ CA.Re pos ] }
            in
            Dict.foldl (\n t -> Dict.union (typeTyvars t)) init attrs


type alias UnifyRecordsFold =
    { aOnly : Dict Name Type
    , bOnly : Dict Name Type
    , both : Dict Name ( Type, Type )
    }


unifyRecords : UnifyReason -> Pos -> List CA.RejectFunction -> ( Maybe String, Dict String Type ) -> ( Maybe String, Dict String Type ) -> Monad Type
unifyRecords reason pos rf ( a_ext, a_attrs ) ( b_ext, b_attrs ) =
    let
        init : UnifyRecordsFold
        init =
            { aOnly = Dict.empty
            , bOnly = Dict.empty
            , both = Dict.empty
            }

        onA : Name -> Type -> UnifyRecordsFold -> UnifyRecordsFold
        onA name type_ state =
            { state | aOnly = Dict.insert name type_ state.aOnly }

        onB : Name -> Type -> UnifyRecordsFold -> UnifyRecordsFold
        onB name type_ state =
            { state | bOnly = Dict.insert name type_ state.bOnly }

        onBoth : Name -> Type -> Type -> UnifyRecordsFold -> UnifyRecordsFold
        onBoth name aType bType state =
            { state | both = Dict.insert name ( aType, bType ) state.both }

        { aOnly, bOnly, both } =
            Dict.merge onA onBoth onB a_attrs b_attrs init
    in
    do (M.dict_map (\k ( a, b ) -> unify_ reason pos rf a b) both) <| \bothUnified ->
    case ( a_ext, b_ext ) of
        ( Just aName, Nothing ) ->
            unifyToNonExtensibleRecord reason aName aOnly bOnly bothUnified

        ( Nothing, Just bName ) ->
            unifyToNonExtensibleRecord reason bName bOnly aOnly bothUnified

        ( Nothing, Nothing ) ->
            if bOnly == Dict.empty && aOnly == Dict.empty then
                -- the two are the same
                return (CA.TypeRecord (CA.I 4) Nothing bothUnified)

            else
                unifyError IncompatibleRecords (CA.TypeRecord pos a_ext a_attrs) (CA.TypeRecord pos b_ext b_attrs)

        ( Just aName, Just bName ) ->
            do (newName identity) <| \new ->
            let
                sub =
                    CA.TypeRecord pos (Just new) (Dict.union bOnly a_attrs)
            in
            do (addSubstitution { overrideIsAnError = True } reason aName rf sub) <| \_ ->
            do (addSubstitution { overrideIsAnError = True } reason bName rf sub) <| \_ ->
            return sub


unifyToNonExtensibleRecord : UnifyReason -> Name -> Dict Name Type -> Dict Name Type -> Dict Name Type -> Monad Type
unifyToNonExtensibleRecord reason aName aOnly bOnly bothUnified =
    if aOnly /= Dict.empty then
        -- b is missing attributes but is not extensible
        Debug.todo "errorRecordDoesNotHaveAttributes b aOnly"

    else
        -- the `a` tyvar should contain the missing attributes, ie `bOnly`
        do (addSubstitution { overrideIsAnError = True } reason aName [ CA.Re (CA.I 7) ] (CA.TypeRecord (CA.I 5) Nothing bOnly)) <| \_ ->
        Dict.union bothUnified bOnly
            |> CA.TypeRecord (CA.I 6) Nothing
            |> return


{-| TODO Rename to type clash?
TODO add position?
-}
unifyError : UnifyError -> Type -> Type -> Monad Type
unifyError error t1 t2 =
    do (newName identity) <| \name ->
    do (insertTypeClash name t1 t2 error) <| \() ->
    return <| CA.TypeVariable (CA.I 0) [] name



----
--- Various helpers
--


applyAttributePath : Pos -> List Name -> Type -> Monad Type
applyAttributePath pos attrPath =
    let
        wrap : Name -> Type -> Monad Type
        wrap attributeName ty =
            let
                -- Unless this is a function parameter, we should know the full type already!
                maybeAttrType =
                    case ty of
                        CA.TypeRecord _ e attrs ->
                            Dict.get attributeName attrs

                        _ ->
                            Nothing
            in
            case maybeAttrType of
                Just attrType ->
                    return attrType

                Nothing ->
                    do (newName identity) <| \extName ->
                    do (newType pos []) <| \attrType ->
                    let
                        re : Type
                        re =
                            CA.TypeRecord (CA.I 2) (Just extName) (Dict.singleton attributeName attrType)
                    in
                    do (unify pos (UnifyReason_AttributeAccess attributeName) ty re) <| \_ ->
                    return attrType
    in
    M.list_foldl wrap attrPath


insertPatternVars : Subs -> Bool -> Bool -> PatternVars -> Env -> Env
insertPatternVars subs isParameter isMutable vars env =
    Dict.foldl (insertPatternVar subs isParameter isMutable) env vars


insertPatternVar : Subs -> Bool -> Bool -> Name -> ( Pos, Type ) -> Env -> Env
insertPatternVar subs isParameter isMutable name ( pos, ty ) env =
    let
        refinedTy =
            replaceTypeVariables subs ty
    in
    { instanceVariables =
        Dict.insert name
            { definedAt = pos
            , ty = refinedTy
            , isMutable = isMutable
            , freeTypeVariables =
                {-
                   Mutables can mutate to any value of a given specific type, so they can't be polymorphic.

                   i.e: `x = Nothing` has type `Maybe a`, but once I mutate it to `@x := Just 1` its type would
                   change to `Maybe Number` and we don't want that.

                   Function parameters instead are **totally determined by how they are used in the function's body**,
                   so they are always added to the scope as non-free type variables.
                   If we turned them into free type variables, the block would not be able to constrain them.

                   Defined instance values are **totally determined by their definition**, how they are used must not
                   affect their type, so each time they are used they can be used with different free type vars.

                   TODO: what about instance variables added by try..as patterns?
                -}
                if isMutable || isParameter then
                    Dict.empty

                else
                    getFreeTypeVars env.constrainedTypeVariables refinedTy
            }
            env.instanceVariables
    , constrainedTypeVariables =
        if isParameter then
            {-

               Within the function body, the type of the parameter must be considered non-free!

               Consider:

                   x q =
                         p = q
                         p

               if the type of `q` remains free, then every time we use `p`, `p` will get an entirely new type.

            -}
            Dict.foldl Dict.insert env.constrainedTypeVariables (typeTyvars refinedTy)

        else
            env.constrainedTypeVariables
    }


applySubsToPatternVarsAndAddThemToEnv : Bool -> PatternVars -> Env -> Monad Env
applySubsToPatternVarsAndAddThemToEnv isMutable vars env =
    do getSubstitutions <| \subs ->
    let
        {-
           Consider:

           x q =
                 { first } = q
                 first

           When we see that `q` must be a record, we need to replace its type.
           However, because the type of `q` is a constrained type variable, we need to make sure that the type(s)
           we use for the records are also constrained!

        -}
        meh typeVarName constrainedVars =
            case Dict.get typeVarName subs of
                Nothing ->
                    constrainedVars

                Just ty ->
                    Dict.foldl Dict.insert constrainedVars (typeTyvars ty)

        env1 =
            { env | constrainedTypeVariables = List.foldl meh env.constrainedTypeVariables (Dict.keys env.constrainedTypeVariables) }
    in
    return <| insertPatternVars subs False isMutable vars env1


replaceTypeVariablesWithNew : InstanceVariable -> Monad Type
replaceTypeVariablesWithNew var =
    if var.freeTypeVariables == Dict.empty then
        return var.ty

    else
        do (generateNewTypeVariables var.freeTypeVariables) <| \newTypeByOldType ->
        return <| replaceTypeVariables newTypeByOldType var.ty


generateNewTypeVariables : Dict Name TypeVariable -> Monad Subs
generateNewTypeVariables tyvarByName =
    let
        apply : Name -> TypeVariable -> Subs -> Monad Subs
        apply name0 tyvar subs =
            do (newName identity) <| \name1 ->
            return <| Dict.insert name0 (CA.TypeVariable tyvar.definedAt tyvar.rf name1) subs
    in
    M.dict_foldl apply tyvarByName Dict.empty


getFreeTypeVars : Dict Name TypeVariable -> Type -> Dict Name TypeVariable
getFreeTypeVars envVars ty =
    Dict.foldl (\name v -> Dict.remove name) (typeTyvars ty) envVars



--FLAG


replaceTypeVariables : Subs -> Type -> Type
replaceTypeVariables subs ty =
    case ty of
        CA.TypeConstant pos ref args ->
            CA.TypeConstant pos ref (List.map (replaceTypeVariables subs) args)

        CA.TypeVariable _ rf name ->
            case Dict.get name subs of
                Just substitutionType ->
                    -- a substitution exists for the variable type v
                    replaceTypeVariables subs substitutionType

                Nothing ->
                    -- no substitution, return the type as-is
                    ty

        CA.TypeFunction pos from fromIsMutable to ->
            CA.TypeFunction pos
                (replaceTypeVariables subs from)
                fromIsMutable
                (replaceTypeVariables subs to)

        CA.TypeAlias pos path t ->
            CA.TypeAlias pos path (replaceTypeVariables subs t)

        CA.TypeRecord pos extensible attrs ->
            case extensible |> Maybe.andThen (\name -> Dict.get name subs) of
                Nothing ->
                    CA.TypeRecord pos extensible (Dict.map (\name -> replaceTypeVariables subs) attrs)

                Just (CA.TypeVariable p rf n) ->
                    CA.TypeRecord pos (Just n) (Dict.map (\name -> replaceTypeVariables subs) attrs)

                Just (CA.TypeRecord _ ext2 attrs2) ->
                    -- attrs2 is the sub, so we give precedence to it
                    Dict.union attrs2 attrs
                        |> Dict.map (\name -> replaceTypeVariables subs)
                        |> CA.TypeRecord pos ext2

                Just what ->
                    Debug.todo "replacing record extension with non-var" (Debug.toString what)


monadError : CA.Pos -> List Error.ContentDiv -> Monad Type
monadError pos message =
    let
        args : Error.ErrorArgs
        args =
            { pos = pos
            , description = \errorEnv -> message
            }
    in
    do (insertError (Error.err args)) <| \() ->
    newType pos []


error_type : Type -> Error.ContentDiv
error_type =
    HCA.typeToString >> Error.text



----
--- Errors
--


errorUndefinedVariable : Pos -> Name -> Monad Type
errorUndefinedVariable pos name =
    monadError pos
        [ Error.text <| "Undefined variable: " ++ name
        ]


errorIncompatibleTypes : UnifyReason -> CA.Pos -> Type -> Dict Name TypeClash -> Monad Type
errorIncompatibleTypes reason pos unifiedType clashes =
    let
        title =
            case reason of
                UnifyReason_AnnotationVsBlock ->
                    "The type of the definition does not match the annotation"

                UnifyReason_AnnotationVsPattern ->
                    "The pattern unpacking is not compatible with the annotation"

                UnifyReason_DefBlockVsPattern ->
                    "The definition block cannot be unpacked into the pattern"

                UnifyReason_CallArgument ->
                    "The function cannot accept arguments of this type"

                UnifyReason_IsBeingCalledAsAFunction ->
                    "This is being called as if it was a function"

                UnifyReason_IfCondition ->
                    "The expression inside `if ... :` should always be a Bool"

                UnifyReason_IfBranches ->
                    "The branches of an `if` should produce the same type of value"

                UnifyReason_TryPattern ->
                    "try..as patterns should have the same type"

                UnifyReason_TryBlock ->
                    "try..as blocks should have the same type"

                UnifyReason_ConstructorArgument p ->
                    "Argument " ++ String.fromInt p.argIndex ++ " to type constructor " ++ p.ref ++ " does not match the constructor definition"

                UnifyReason_AttributeAccess attrName ->
                    "You are trying to access the ." ++ attrName ++ " attribute"

                UnifyReason_AttributeUpdate attrNames ->
                    "You are trying to update the " ++ String.join ", " attrNames ++ " attributes"
    in
    case unifiedType of
        CA.TypeVariable p rf unifiedTypeName ->
            [ [ Error.text title ]
            , clashes
                |> Dict.toList
                |> List.concatMap
                    (\( clashPlaceholderName, clash ) ->
                        [ Error.text <| Debug.toString clash.err
                        , error_type clash.t1
                        , error_type clash.t2
                        ]
                    )
            ]
                |> List.concat
                |> monadError pos

        _ ->
            let
                info =
                    -- TODO the layout should change depending on the reason, not only the title
                    [ Error.text "The type seems to be something like"
                    , error_type unifiedType
                    , Error.text ""
                    , Error.text "However I can't reconcile the following:"
                    ]

                --clashToError : ( Id, TypeClash ) -> List String
                clashToError ( name, clash ) =
                    [ Error.text ""
                    , Error.text <| "`" ++ name ++ "` can be two incompatible types:"
                    , error_type clash.t1
                    , error_type clash.t2
                    , Error.text <| "(" ++ Debug.toString clash.err ++ ")"
                    ]

                clashErrors =
                    clashes
                        |> Dict.toList
                        |> List.concatMap clashToError
            in
            [ [ Error.text title ]
            , info
            , clashErrors
            ]
                |> List.concat
                |> monadError pos


errorTodo : CA.Pos -> String -> Monad Type
errorTodo pos message =
    monadError pos [ Error.text message ]



--
-- TODO Type validation is not really part of type inference, does it make sense to keep it in this module?
--
-- Type Validation
--
{-
   validateType : List (Pos, String) -> Type -> (Pos, List (Pos, String))
   validateType nonFunctionReasons ty =
       case ty of
           CA.TypeConstant _ _ _ ->
               Nothing

           CA.TypeVariable _ name ->
               {- TODO
                   if mutable then
                       Just "variable types can't be mutable"

                  ----> except they can, they must be if we want to have functions
                  capable of manipulating mutable containers

               -}
               Nothing

           CA.TypeAlias _ path t ->
               validateType nonFunctionReasons t

           CA.TypeFunction _ from fromIsMutable to ->
               case nonFunctionReasons of
                   [] ->
                       case validateType fromIsMutable from of
                           Just e ->
                               Just e

                           Nothing ->
                               validateType False to

                   _ ->
                       nonFunctionReasons


           CA.TypeRecord _ ext attrs ->
               attrs
                   -- TODO Rewrite the whole dumpster fire to support returning  multiple errors
                   |> Dict.values
                   |> List.filterMap (validateType nonFunctionReason)
                   |> List.head
-}
--
-- Add union type constructors to the Env
--
-- TODO I'm not sure that this should live in this module
--


addConstructors : CA.UnionDef -> Env -> Env
addConstructors def env =
    Dict.foldl (addConstructor def) env def.constructors


addConstructor : CA.UnionDef -> Name -> List Type -> Env -> Env
addConstructor unionDef ctorName ctorArgs env =
    let
        args =
            List.map (\name -> CA.TypeVariable (CA.I 8) [] name) unionDef.args

        fold ty accum =
            CA.TypeFunction (CA.I 9) ty False accum

        ctorType =
            List.foldr fold (CA.TypeConstant (CA.I 10) unionDef.name args) ctorArgs

        nameToTyvar : Name -> TypeVariable
        nameToTyvar name =
            { definedAt = CA.U
            , rf = []
            }

        var : InstanceVariable
        var =
            { ty = ctorType
            , definedAt = CA.I 11
            , freeTypeVariables = List.foldl (\n -> Dict.insert n (nameToTyvar n)) Dict.empty unionDef.args
            , isMutable = False
            }
    in
    { env | instanceVariables = Dict.insert ctorName var env.instanceVariables }


allDefsToEnvAndValues : Dict Name CA.RootDef -> ( Env, List CA.RootValueDef )
allDefsToEnvAndValues allDefs =
    let
        ( aliases, unions, values ) =
            CA.split allDefs
    in
    ( Dict.foldl (\name -> addConstructors) initEnv unions
    , Dict.values values
    )
