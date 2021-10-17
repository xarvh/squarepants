module Compiler.TypeCheck exposing (..)

{-| This module implements what is think is the (Damas)-Hindley-Milner inference algorithm, but I'm not sure.


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
import Human.CanonicalAst
import RefHierarchy
import Set exposing (Set)
import StateMonad as M exposing (M, do, get, return)
import Types.CanonicalAst as CA exposing (Pos, Type)
import Types.Error as Error exposing (Error, Res)
import Types.Literal


typeToText =
    Human.CanonicalAst.typeToString


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

    {-
       Every time we use a value in an expression, we must re-instantiate its free variables, because each time they can be used in a different way.

       Because of this each Definition has a `freeTypeVariables` field.

       To figure out `freeTypeVariables` we can't simply take all tyvars we find in the definition's type, because:

       1) a tyvar that is free in a parent definition should not be considered free in children definitions:

         parent =
           as List a

           child =
             as a -> a
             ...

           ...

       2) the placeholder tyvar of an argument should not be considered free while we infer the type of a function, otherwise we keep reinstantitiating it and we never figure out anything.

         f x =
           { first } = x
           first

       By keeping track of which tyvars should NOT be considered free, we can figure out the correct `freeTypeVariables` field for each child definition.
    -}
    , nonFreeTyvars : Dict Name Pos

    -- This is used to produce nicer errors for when a recursive function is not annotated
    , nonAnnotatedRecursives : Dict Name Pos
    }


initEnv : Env
initEnv =
    { instanceVariables = Dict.empty
    , nonFreeTyvars = Dict.empty
    , nonAnnotatedRecursives = Dict.empty
    }


type alias InstanceVariable =
    { definedAt : CA.Pos
    , ty : Type
    , freeTypeVariables : Dict Name { nonFn : Bool }
    , isMutable : Bool
    }



----
--- State
--


type alias State =
    { nextName : Int
    , errors : List Error
    , substitutions : Subs

    -- Type variables that cannot accept functions
    , nonFnTyvars : Dict Name (List CA.RejectFunction)

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
    , nonFnTyvars = Dict.empty
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


newType : Pos -> Monad Type
newType pos =
    newName (CA.TypeVariable pos)


insertError : Error -> Monad ()
insertError e state =
    ( ()
    , { state | errors = e :: state.errors }
    )


setNonFn : Name -> Monad ()
setNonFn name state =
    ( ()
      -- TODO value should not be an empty list?
    , { state | nonFnTyvars = Dict.insert name [] state.nonFnTyvars }
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


makeNative : CA.Annotation -> CA.RootValueDef -> InstanceVariable
makeNative ann def =
    { definedAt = def.pos
    , ty = ann.ty
    , freeTypeVariables = getFreeTypeVars Dict.empty ann.nonFn ann.ty
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

        Just ann ->
            { env
                | instanceVariables =
                    Dict.insert def.name
                        { definedAt = def.pos
                        , ty = ann.ty
                        , freeTypeVariables = getFreeTypeVars Dict.empty ann.nonFn ann.ty
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
            if typeContainsFunctions stateF.inferredType then
                addError head [ "blocks that define mutables can't return functions" ]

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
    do (get .substitutions) <| \subs ->
    return <| replaceTypeVariables subs ty


fromDefinition : Env -> Bool -> CA.Pattern -> Maybe CA.Annotation -> List CA.Statement -> Monad Env
fromDefinition env isMutable pattern maybeAnnotation body =
    case maybeAnnotation of
        Nothing ->
            -- No annotation: recursive definitions are not allowed
            -- This means that *first* we infer the definition, *then* we add it to the environment
            --
            -- TODO ability to infer recursive definitions, but only if I don't have to rewrite everything
            --
            do (fromPattern env pattern Dict.empty) <| \patternOut ->
            let
                env1 =
                    { env | nonAnnotatedRecursives = Dict.foldl (\name ( pos, ty ) -> Dict.insert name pos) env.nonAnnotatedRecursives patternOut.vars }
            in
            do (fromBlock env1 body) <| \bodyType_ ->
            do (applySubsToType bodyType_) <| \bodyType ->
            do (unify patternOut.pos UnifyReason_DefBlockVsPattern bodyType patternOut.ty) <| \unifiedType ->
            do (applySubsToPatternVarsAndAddThemToEnv isMutable patternOut.vars env) <| return

        Just annotation ->
            -- There is an annotation: allow recursive definitions
            -- This means that we can add it to the environment before we actually infer it
            do (fromPattern env pattern Dict.empty) <| \patternOut ->
            do (unify patternOut.pos UnifyReason_AnnotationVsPattern annotation.ty patternOut.ty) <| \_ ->
            -- applySubsToPatternVarsAndAddThemToEnv also adds type variables, which is why we're using it here!
            -- TODO maybe I should make it more explicit? Use an ad-hoc function?
            do (applySubsToPatternVarsAndAddThemToEnv isMutable patternOut.vars env) <| \env1 ->
            do (fromBlock env1 body) <| \bodyType ->
            do (unify patternOut.pos UnifyReason_AnnotationVsBlock annotation.ty bodyType) <| \unifiedBlockType ->
            do (checkFreeVariables patternOut.pos annotation.ty bodyType) <| \() ->
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
            (addError pos
                [ "The annotation is too general"
                , ""
                , "The annotation uses: " ++ String.join ", " (Dict.keys annotatedFreeVars)
                , typeToText annotatedType
                , ""
                , "But the actual type uses only: " ++ String.join ", " (Dict.keys actualFreeVars)
                , typeToText blockType
                , ""
                , "The annotation has " ++ String.fromInt (Dict.size annotatedFreeVars - Dict.size actualFreeVars) ++ " type variables too many"
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
                    errorUndefinedVariable env pos name

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
            do (insertPatternVars Dict.empty True isMutable patternOut.vars env) <| \bodyEnv ->
            do (fromBlock bodyEnv body) <| \bodyType ->
            -- fromParameter can infer paramType only when destructuring some patterns, it's not reliable in general
            -- fromBlock instead infers paramType fully, but will not apply the substitutions it creates
            -- So we just pull out the substitutions and apply them to paramType
            do (applySubsToType patternOut.ty) <| \refinedPatternOutTy ->
            if isMutable && typeContainsFunctions refinedPatternOutTy then
                -- TODO be a bit more descriptive, maybe name the arguments
                errorTodo pos <| "mutable args cannot be functions"

            else
                CA.TypeFunction pos refinedPatternOutTy isMutable bodyType
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
            do (get .substitutions) <| \s ->
            do (fromBlock env ar.true) <| \trueType ->
            do (fromBlock env ar.false) <| \falseType ->
            unify pos UnifyReason_IfBranches trueType falseType

        CA.Try pos value patternsAndBlocks ->
            do (fromExpression env value) <| \tryType ->
            do (newType pos) <| \newBlockType ->
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
                addError pos [ "mutability clash 2" ]

            else
                do (unify pos (UnifyReason_CallArgument pos) refArgumentType callArgumentType) <| \unifiedArgumentType ->
                applySubsToType refReturnType

        CA.TypeVariable _ name ->
            do (newType pos) <| \returnType ->
            let
                ty =
                    CA.TypeFunction pos callArgumentType callIsMutable returnType
            in
            do (unify pos UnifyReason_IsBeingCalledAsAFunction callReferenceType ty) <| \_ ->
            applySubsToType returnType

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
                    do (errorUndefinedVariable env pos name) <| \ty ->
                    return ( True, ty )

                Just var ->
                    if not var.isMutable then
                        do (errorTryingToMutateAnImmutable pos name) <| \ty ->
                        return ( True, ty )

                    else if typeContainsFunctions var.ty then
                        -- TODO what about constrained/unconstrained tyvars?
                        do (addError pos [ "mutable arguments can't allow functions" ]) <| \ty ->
                        return ( True, ty )

                    else
                        do (applyAttributePath pos attrPath var.ty) <| \ty ->
                        return ( True, ty )


errorTryingToMutateAnImmutable : Pos -> Name -> Monad Type
errorTryingToMutateAnImmutable pos name =
    addError pos
        [ "You are trying to mutate variable `" ++ name ++ "` but it was declared as not mutable!"
        , ""
        , "TODO [link to wiki page that explains how to declare variables]"
        ]


fromParameter : Env -> CA.Parameter -> Monad ( Bool, PatternOut )
fromParameter env param =
    case param of
        CA.ParameterPattern pattern ->
            do (fromPattern env pattern Dict.empty) <| \patternOut ->
            return ( False, patternOut )

        CA.ParameterMutable pos paramName ->
            -- TypeNonFunction
            do (newType pos) <| \ty ->
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
            do (newType pos) <| \ty ->
            return <| PatternOut vars pos ty

        CA.PatternAny pos name ->
            do (newType pos) <| \ty ->
            return <| PatternOut (Dict.insert name ( pos, ty ) vars) pos ty

        CA.PatternLiteral pos literal ->
            return <| PatternOut vars pos (fromLiteral literal)

        CA.PatternConstructor pos ref args ->
            case Dict.get ref env.instanceVariables of
                Nothing ->
                    -- TODO still add all variables defined in the args, otherwise there will
                    -- missing variables that will trigger misleading "undefined variable" errors
                    -- (ie, use unifyConstructorWithItsArgs anyway)
                    do (errorUndefinedVariable env pos ref) <| \ety ->
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
            do (addError p.pos [ "Type constructor " ++ p.ref ++ " is missing an argument " ++ String.fromInt p.argIndex ]) <| \ety ->
            return ( p.vars, ety )

        -- No arguments needed, no arguments given
        ( _, [] ) ->
            do (get .substitutions) <| \subs ->
            -- TODO should I apply subs here?
            return ( p.vars, p.ty )

        -- Error: no argument needed, but argument given!
        ( _, head :: tail ) ->
            -- TODO tell how many are needed and how many are actually given
            do (addError p.pos [ "Type constructor " ++ p.ref ++ " has too many args" ]) <| \ety ->
            return ( p.vars, ety )



----
--- Unification
--


type UnifyReason
    = UnifyReason_AnnotationVsPattern
    | UnifyReason_AnnotationVsBlock
    | UnifyReason_DefBlockVsPattern
    | UnifyReason_CallArgument Pos
    | UnifyReason_IsBeingCalledAsAFunction
    | UnifyReason_IfCondition
    | UnifyReason_IfBranches
    | UnifyReason_TryPattern
    | UnifyReason_TryBlock
    | UnifyReason_ConstructorArgument UnifyConstructorWithItsArgsParams
    | UnifyReason_AttributeAccess Name
    | UnifyReason_AttributeUpdate (List Name)
    | UnifyReason_Override


type UnifyError
    = IncompatibleTypes
    | IncompatibleMutability
    | IncompatibleRecords (List Name) (List Name) (List Name)
    | Cycle Name
    | NotAFunction
    | NonFunctionContainsFunction (List CA.RejectFunction)
    | OkThisIsActuallyPossible
    | NI String


unify : CA.Pos -> UnifyReason -> Type -> Type -> Monad Type
unify pos reason a b =
    do (unify_ reason pos a b) <| \unifiedType ->
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
unify_ : UnifyReason -> Pos -> Type -> Type -> Monad Type
unify_ reason pos1 t1 t2 =
    case ( t1, t2 ) of
        ( CA.TypeAlias pos _ aliased, _ ) ->
            unify_ reason pos aliased t2

        ( _, CA.TypeAlias _ _ aliased ) ->
            unify_ reason pos1 t1 aliased

        ( CA.TypeConstant pos ref1 args1, CA.TypeConstant _ ref2 args2 ) ->
            if ref1 /= ref2 then
                unifyError IncompatibleTypes t1 t2

            else
                let
                    fold arg1 arg2 =
                        unify_ reason pos arg1 arg2
                in
                -- TODO should I check arity here?
                do (M.list_map2 (unify_ reason pos) args1 args2) <| \argTypes ->
                do (get .substitutions) <| \subs ->
                argTypes
                    |> List.map (replaceTypeVariables subs)
                    |> CA.TypeConstant pos ref1
                    |> return

        ( CA.TypeVariable pos v1_name, CA.TypeVariable _ v2_name ) ->
            if v1_name == v2_name then
                return t1

            else
                do (get .substitutions) <| \subs ->
                case ( Dict.get v1_name subs, Dict.get v2_name subs ) of
                    ( Just sub1, Just sub2 ) ->
                        do (unify_ reason pos1 sub1 sub2) <| \v ->
                        -- I think override here is False because it was used to propagate NonFunction in one of the attempted implementations
                        do (addSubstitution "vv1" pos reason v1_name v) <| \_ ->
                        do (addSubstitution "vv2" pos reason v2_name v) <| \subbedTy ->
                        return subbedTy

                    ( Nothing, Just sub2 ) ->
                        addSubstitution "vv3" pos reason v1_name t2

                    _ ->
                        addSubstitution "vv4" pos reason v2_name t1

        ( CA.TypeVariable pos name1, _ ) ->
            addSubstitution "vl" pos reason name1 t2

        ( _, CA.TypeVariable pos name2 ) ->
            addSubstitution "vr" pos reason name2 t1

        ( CA.TypeFunction pos a_from a_fromIsMutable a_to, CA.TypeFunction _ b_from b_fromIsMutable b_to ) ->
            if a_fromIsMutable /= b_fromIsMutable then
                unifyError IncompatibleMutability t1 t2

            else
                do (unify_ reason pos a_from b_from) <| \unified_from ->
                do (get .substitutions) <| \subs_ ->
                {- Without the replacement here, a function annotation will produce circular substitutions

                   id a =
                       is a -> a
                       a

                -}
                do (unify_ reason pos (replaceTypeVariables subs_ a_to) (replaceTypeVariables subs_ b_to)) <| \unified_to ->
                do (get .substitutions) <| \subs ->
                CA.TypeFunction pos (replaceTypeVariables subs unified_from) a_fromIsMutable (replaceTypeVariables subs unified_to)
                    |> return

        ( CA.TypeRecord _ a_ext a_attrs, CA.TypeRecord _ b_ext b_attrs ) ->
            unifyRecords reason pos1 ( a_ext, a_attrs ) ( b_ext, b_attrs )

        _ ->
            unifyError IncompatibleTypes t1 t2


type alias UnifyRecordsFold =
    { aOnly : Dict Name Type
    , bOnly : Dict Name Type
    , both : Dict Name ( Type, Type )
    }


unifyRecords : UnifyReason -> Pos -> ( Maybe String, Dict String Type ) -> ( Maybe String, Dict String Type ) -> Monad Type
unifyRecords reason pos ( a_ext, a_attrs ) ( b_ext, b_attrs ) =
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
    do (M.dict_map (\k ( a, b ) -> unify_ reason pos a b) both) <| \bothUnified ->
    case ( a_ext, b_ext ) of
        ( Just aName, Nothing ) ->
            unifyToNonExtensibleRecord pos reason aName aOnly bOnly bothUnified

        ( Nothing, Just bName ) ->
            unifyToNonExtensibleRecord pos reason bName bOnly aOnly bothUnified

        ( Nothing, Nothing ) ->
            if bOnly == Dict.empty && aOnly == Dict.empty then
                -- the two are the same
                return (CA.TypeRecord (CA.I 4) Nothing bothUnified)

            else
                let
                    e =
                        IncompatibleRecords
                            (Dict.keys bOnly)
                            (Dict.keys aOnly)
                            (Dict.keys bothUnified)
                in
                unifyError e (CA.TypeRecord pos a_ext a_attrs) (CA.TypeRecord pos b_ext b_attrs)

        ( Just aName, Just bName ) ->
            if aName == bName && aOnly == Dict.empty && bOnly == Dict.empty then
                return <| CA.TypeRecord pos (Just aName) bothUnified

            else
                do (newName identity) <| \new ->
                let
                    sub =
                        CA.TypeRecord pos (Just new) (Dict.union bOnly a_attrs)
                in
                do (addSubstitution "jj1" pos reason aName sub) <| \_ ->
                do (addSubstitution "jj2" pos reason bName sub) <| \_ ->
                return sub


unifyToNonExtensibleRecord : Pos -> UnifyReason -> Name -> Dict Name Type -> Dict Name Type -> Dict Name Type -> Monad Type
unifyToNonExtensibleRecord pos reason aName aOnly bOnly bothUnified =
    if aOnly /= Dict.empty then
        -- b is missing attributes but is not extensible
        addError pos
            [ "record is missing attrs: " ++ (aOnly |> Dict.keys |> String.join ", ")
            , Debug.toString reason
            ]

    else
        -- the `a` tyvar should contain the missing attributes, ie `bOnly`
        do (newName Just) <| \ext ->
        do (addSubstitution "ne" pos reason aName (CA.TypeRecord (CA.I 5) ext bOnly)) <| \_ ->
        Dict.union bothUnified bOnly
            |> CA.TypeRecord pos Nothing
            |> return


{-| TODO Rename to type clash?
TODO add position?
-}
unifyError : UnifyError -> Type -> Type -> Monad Type
unifyError error t1 t2 =
    do (newName identity) <| \name ->
    do (insertTypeClash name t1 t2 error) <| \() ->
    return <| CA.TypeVariable (CA.I 0) name



----
--- Add substitutions
--


addSubstitution : String -> Pos -> UnifyReason -> Name -> Type -> Monad Type
addSubstitution debugCode pos reason name rawTy =
    do (applySubsToType rawTy) <| \ty ->
    let
        x =
            ( name, ty, rawTy )

        _ =
            if String.toInt name == Nothing then
                Debug.log "addSubstitution on annotated tyvar" x

            else
                x
    in
    if typeHasTyvar name ty then
        -- TODO This feels a bit like a hacky work around.
        -- Maybe it's because I don't call applySubsToType enough before calling unify?
        -- Then again, it feels more robust?
        -- Too much feeling, not enough understanding.
        if typeIsTyvar name ty then
            return ty

        else
            unifyError (Cycle name) (CA.TypeVariable pos name) ty


    else
        do (checkNonFunction name ty) <| \{ freeVarsToFlag } ->
        do (flagFreeVars freeVarsToFlag) <| \_ ->
        do (get .substitutions) <| \subs ->
        case Dict.get name subs of
            {-
               The tyvar has already been substituted
               This means that it has been already applied to all other subs and `name` does not appear in the subs.

               Is this enough to exclude infinite calls between unify_ and addSubstitution?
            -}
            Just sub ->
                unify_ reason pos ty sub

            Nothing ->
                -- apply new substitution to all old substitutions
                \state ->
                    ( ty
                    , { state
                        | substitutions =
                            state.substitutions
                                |> Dict.map (\k -> replaceTypeVariables (Dict.singleton name ty))
                                |> Dict.insert name ty
                      }
                    )


typeIsTyvar : Name -> Type -> Bool
typeIsTyvar name ty =
    case ty of
        CA.TypeVariable _ n ->
            n == name

        _ ->
            False


checkNonFunction : Name -> Type -> Monad { freeVarsToFlag : List Name }
checkNonFunction name ty =
    let
        nope =
            { freeVarsToFlag = [] }
    in
    do (get .nonFnTyvars) <| \nonFnTyvars ->
    case Dict.get name nonFnTyvars of
        Nothing ->
            return nope

        Just rejectReasons ->
            -- type should not contain function
            if typeContainsFunctions ty then
                {- TODO how do I talk about this type?

                   "The type of `f`"?

                -}
                do (errorTodo (CA.I 26) <| "type `" ++ name ++ "` should not contain functions, but is " ++ typeToText ty) <| \_ ->
                return nope

            else
                -- blah!
                -- TODO constrained vars inside ty should all reject functions
                -- TODO non-constraned vars should be flagged with reject function
                return nope


flagFreeVars : List Name -> Monad ()
flagFreeVars names =
    -- TODO!!!!
    return ()



----
--- Various helpers
--


typeContainsFunctions : Type -> Bool
typeContainsFunctions ty =
    case ty of
        CA.TypeConstant _ _ args ->
          List.any typeContainsFunctions args

        CA.TypeVariable _ _ ->
            False

        CA.TypeFunction _ from fromIsMutable to ->
            True

        CA.TypeAlias _ path t ->
            typeContainsFunctions t

        CA.TypeRecord _ extensible attrs ->
            attrs
                |> Dict.values
                |> List.any typeContainsFunctions


typeHasTyvar : Name -> Type -> Bool
typeHasTyvar n ty =
    case ty of
        CA.TypeVariable pos name ->
            n == name

        CA.TypeFunction _ from fromIsMutable to ->
            typeHasTyvar n from || typeHasTyvar n to

        CA.TypeConstant pos ref args ->
            List.any (typeHasTyvar n) args

        CA.TypeAlias _ path t ->
            typeHasTyvar n t

        CA.TypeRecord pos extensible attrs ->
            Just n == extensible || List.any (typeHasTyvar n) (Dict.values attrs)


typeTyvars : Type -> Dict Name Pos
typeTyvars ty =
    case ty of
        CA.TypeVariable pos name ->
            -- TODO is pos equivalent to definedAt?
            Dict.singleton name pos

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
                            Dict.singleton name pos
            in
            Dict.foldl (\n t -> Dict.union (typeTyvars t)) init attrs


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
                    do (newType pos) <| \attrType ->
                    let
                        re : Type
                        re =
                            CA.TypeRecord (CA.I 2) (Just extName) (Dict.singleton attributeName attrType)
                    in
                    do (unify pos (UnifyReason_AttributeAccess attributeName) ty re) <| \_ ->
                    return attrType
    in
    M.list_foldl wrap attrPath


insertPatternVars : Subs -> Bool -> Bool -> PatternVars -> Env -> Monad Env
insertPatternVars subs isParameter isMutable =
    M.dict_foldl (insertPatternVar subs isParameter isMutable)


insertPatternVar : Subs -> Bool -> Bool -> Name -> ( Pos, Type ) -> Env -> Monad Env
insertPatternVar subs isParameter isMutable name ( pos, ty ) env =
    let
        refinedTy =
            replaceTypeVariables subs ty
    in
    { nonAnnotatedRecursives = env.nonAnnotatedRecursives
    , instanceVariables =
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
                    getFreeTypeVars env.nonFreeTyvars Dict.empty refinedTy
            }
            env.instanceVariables
    , nonFreeTyvars =
        if isParameter then
            {-

               Within the function body, the type of the parameter must be considered non-free!

               Consider:

                   x q =
                         p = q
                         p

               if the type of `q` remains free, then every time we use `p`, `p` will get an entirely new type.

            -}
            Dict.foldl Dict.insert env.nonFreeTyvars (typeTyvars refinedTy)

        else
            env.nonFreeTyvars
    }
        |> return


applySubsToPatternVarsAndAddThemToEnv : Bool -> PatternVars -> Env -> Monad Env
applySubsToPatternVarsAndAddThemToEnv isMutable vars env =
    do (get .substitutions) <| \subs ->
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
        meh : Name -> Dict Name Pos -> Dict Name Pos
        meh typeVarName constrainedVars =
            case Dict.get typeVarName subs of
                Nothing ->
                    constrainedVars

                Just ty ->
                    Dict.foldl (\n p -> Dict.insert n p) constrainedVars (typeTyvars ty)

        env1 =
            { env | nonFreeTyvars = List.foldl meh env.nonFreeTyvars (Dict.keys env.nonFreeTyvars) }
    in
    insertPatternVars subs False isMutable vars env1


replaceTypeVariablesWithNew : InstanceVariable -> Monad Type
replaceTypeVariablesWithNew var =
    if var.freeTypeVariables == Dict.empty then
        return var.ty

    else
        do (generateNewTypeVariables var.freeTypeVariables) <| \newTypeByOldType ->
        return <| replaceTypeVariables newTypeByOldType var.ty


generateNewTypeVariables : Dict Name { nonFn : Bool } -> Monad Subs
generateNewTypeVariables tyvarByName =
    let
        apply : Name -> { nonFn : Bool } -> Subs -> Monad Subs
        apply name0 { nonFn } subs =
            do (newName identity) <| \name1 ->
            do
                (if nonFn then
                    setNonFn name1

                 else
                    return ()
                )
            <| \() ->
            return <| Dict.insert name0 (CA.TypeVariable (CA.I 11) name1) subs
    in
    M.dict_foldl apply tyvarByName Dict.empty


getFreeTypeVars : Dict Name Pos -> Dict Name Pos -> Type -> Dict Name { nonFn : Bool }
getFreeTypeVars nonFreeTyvars nonFn ty =
    let
        --posToTyvar : Name -> Pos -> TypeVariable
        posToTyvar name pos =
            { nonFn = Dict.member name nonFn }
    in
    Dict.diff (typeTyvars ty) nonFreeTyvars
        |> Dict.map posToTyvar



--FLAG


replaceTypeVariables : Subs -> Type -> Type
replaceTypeVariables subs ty =
    case ty of
        CA.TypeConstant pos ref args ->
            CA.TypeConstant pos ref (List.map (replaceTypeVariables subs) args)

        CA.TypeVariable _ name ->
            case Dict.get name subs of
                Just substitutionType ->
                    -- addSubstitution always applies all subs to each sub's type, so we don't need to apply them here
                    substitutionType

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

                Just (CA.TypeVariable p n) ->
                    CA.TypeRecord pos (Just n) (Dict.map (\name -> replaceTypeVariables subs) attrs)

                Just (CA.TypeRecord _ ext2 attrs2) ->
                    -- attrs2 is the sub, so we give precedence to it
                    Dict.union attrs2 attrs
                        |> Dict.map (\name -> replaceTypeVariables subs)
                        |> CA.TypeRecord pos ext2

                Just what ->
                    Debug.todo "replacing record extension with non-var" (Debug.toString what)


addError : CA.Pos -> List String -> Monad Type
addError pos message =
    addErrorWithEEnv pos (always message)


addErrorWithEEnv : CA.Pos -> (Error.ErrorEnv -> List String) -> Monad Type
addErrorWithEEnv pos messageConstructor =
    do (insertError (Error.markdown pos messageConstructor)) <| \() ->
    newType pos



----
--- Errors
--
{- TODO this should go somewhere else -}


splitName : String -> ( Maybe String, String )
splitName s =
    case String.split "." s of
        moduleName :: valueName :: [] ->
            ( Just moduleName, valueName )

        _ ->
            ( Nothing, s )


errorUndefinedVariable : Env -> Pos -> Name -> Monad Type
errorUndefinedVariable env pos normalizedName =
    addErrorWithEEnv pos <| \errorEnv ->
    let
        rawName =
            Error.posToToken errorEnv pos

        ( maybeRawModuleName, name ) =
            splitName rawName

        ( maybeNormalizedModuleName, _ ) =
            splitName normalizedName
    in
    case Dict.get name env.nonAnnotatedRecursives of
        Nothing ->
            case ( maybeRawModuleName, maybeNormalizedModuleName ) of
                ( Just rawModuleName, Just normalizedModuleName ) ->
                    case Dict.get normalizedModuleName errorEnv.moduleByName of
                        Just _ ->
                            [ "Module `" ++ normalizedModuleName ++ "` does not seem to expose a variable called `" ++ name ++ "`."
                            ]

                        Nothing ->
                            [ "I can't find any module called `" ++ normalizedModuleName ++ "`."
                            ]

                _ ->
                    [ "Undefined value: " ++ name
                    , ""
                    , "I can't see a definition for `" ++ name ++ "` anywhere, so I don't know what it is."
                    ]

        Just defPos ->
            [ "To use function `" ++ name ++ "` recursively, you need to add a type annotation to its definition."
            , ""
            , "This is a limit of the compiler, not sure when I'll have the time to fix it."
            ]


errorIncompatibleTypes : UnifyReason -> CA.Pos -> Type -> Dict Name TypeClash -> Monad Type
errorIncompatibleTypes reason pos_whatever unifiedType clashes =
    let
        pos =
            case reason of
                UnifyReason_CallArgument p ->
                    p

                _ ->
                    pos_whatever

        title =
            case reason of
                UnifyReason_AnnotationVsBlock ->
                    "The type of the definition does not match the annotation"

                UnifyReason_AnnotationVsPattern ->
                    "The pattern unpacking is not compatible with the annotation"

                UnifyReason_DefBlockVsPattern ->
                    "The definition block cannot be unpacked into the pattern"

                UnifyReason_CallArgument _ ->
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

                UnifyReason_Override ->
                    "this is addSubstitution running a UnifyReason_Override, I don't know what I'm doing"
    in
    case unifiedType of
        CA.TypeVariable p unifiedTypeName ->
            [ [ title ]
            , clashes
                |> Dict.toList
                |> List.concatMap
                    (\( clashPlaceholderName, clash ) ->
                        [ Debug.toString clash.err
                        , typeToText clash.t1
                        , typeToText clash.t2
                        ]
                    )
            ]
                |> List.concat
                |> addError pos

        _ ->
            let
                info =
                    -- TODO the layout should change depending on the reason, not only the title
                    [ "The type seems to be something like"
                    , typeToText unifiedType
                    , ""
                    , "However I can't reconcile the following:"
                    ]

                --clashToError : ( Id, TypeClash ) -> List String
                clashToError ( name, clash ) =
                    [ ""
                    , "`" ++ name ++ "` can be two incompatible types:"
                    , ""
                    , typeToText clash.t1
                    , ""
                    , typeToText clash.t2
                    , ""
                    , "(" ++ Debug.toString clash.err ++ ")"
                    ]

                clashErrors =
                    clashes
                        |> Dict.toList
                        |> List.concatMap clashToError
            in
            [ [ title ]
            , info
            , clashErrors
            ]
                |> List.concat
                |> addError pos


errorTodo : CA.Pos -> String -> Monad Type
errorTodo pos message =
    addError pos [ message ]



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
            List.map (\name -> CA.TypeVariable (CA.I 8) name) unionDef.args

        fold ty accum =
            CA.TypeFunction (CA.I 9) ty False accum

        ctorType =
            List.foldr fold (CA.TypeConstant (CA.I 10) unionDef.name args) ctorArgs

        var : InstanceVariable
        var =
            { ty = ctorType
            , definedAt = CA.I 11
            , freeTypeVariables = List.foldl (\n -> Dict.insert n { nonFn = False }) Dict.empty unionDef.args
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
