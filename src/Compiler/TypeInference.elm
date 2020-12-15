module Compiler.TypeInference exposing (..)

import Dict exposing (Dict)
import Generator as TyGen
import Html
import Set exposing (Set)
import Types.CanonicalAst as CA exposing (Type)


type alias Res a =
    Result String a


type alias Substitutions =
    Dict String Type


type alias Env =
    -- (Ty, Set String) is a type's "schema"?
    --
    -- Set String is the set of all forall vars in the type!
    --
    Dict String ( Type, Set String )



----
--- Variable type generator
--


type alias TyGen a =
    TyGen.Generator Int a

newName : TyGen String
newName =
    TyGen.next ((+) 1) (\n -> "t" ++ String.fromInt n )

newType : TyGen Type
newType =
    TyGen.map (\s -> CA.TypeVariable { name = s }) newName


do_nr : TyGen (Res a) -> (a -> TyGen (Res b)) -> TyGen (Res b)
do_nr nra f =
    TyGen.do nra
        (\ra ->
            case ra of
                Ok a ->
                    f a

                Err e ->
                    TyGen.wrap (Err e)
        )


do_res a b =
    Result.andThen b a



----
--- Core types
--


typeNone : Type
typeNone =
    CA.TypeConstant { name = "None" }



-- TODO add constructors


typeBool : Type
typeBool =
    CA.TypeConstant { name = "Bool" }



----
--- Modules
--


inspectModule : Env -> CA.Module e -> Res Env
inspectModule prelude mod =
    let
        statements =
            mod.valueDefinitions
                |> Dict.values
                |> List.map CA.Definition

        env =
            -- TODO add type constructors to env!
            prelude

        gen =
            do_nr (inspectStatementList statements env Dict.empty) <| \( shouldBeNone, env1, subs ) ->
            refine_env subs env1
                |> Ok
                |> TyGen.wrap

        ( envResult, nextId ) =
            TyGen.run 0 gen
    in
    envResult



----
--- Substitutions
--


refine_type : Substitutions -> Type -> Type
refine_type subs ty =
    case ty of
        CA.TypeConstant _ ->
            -- TODO what about type parameters?
            ty

        CA.TypeVariable { name } ->
            case Dict.get name subs of
                Just substitutionType ->
                    -- a substitution exists for the variable type v
                    refine_type subs substitutionType

                Nothing ->
                    -- no substitution, return the type as-is
                    ty

        CA.TypeFunction { from, fromIsMutable, to } ->
            CA.TypeFunction
                { from = refine_type subs from
                , fromIsMutable = fromIsMutable
                , to = refine_type subs to
                }

        CA.TypeRecord attrs ->
            attrs
                |> List.map (\a -> { a | type_ = refine_type subs a.type_ })
                |> CA.TypeRecord


tyvars_from_type : Type -> Set String
tyvars_from_type ty =
    case ty of
        CA.TypeVariable { name } ->
            Set.singleton name

        CA.TypeFunction { from, to } ->
            Set.union (tyvars_from_type from) (tyvars_from_type to)

        CA.TypeConstant _ ->
            Set.empty

        CA.TypeRecord attrs ->
            List.foldl (\a -> Set.union (tyvars_from_type a.type_)) Set.empty attrs



----
--- Env
--


{-| string -> ty -> env -> env
-}
env_add : String -> ( Type, Set String ) -> Env -> Env
env_add v sc e =
    Dict.insert v sc e


instantiate_type : Type -> Set String -> TyGen Type
instantiate_type t tvars =
    let
        aggregate : String -> TyGen Substitutions -> TyGen Substitutions
        aggregate tvar nextSubsAcc =
            TyGen.do newType <| \nt ->
            TyGen.map (Dict.insert tvar nt) nextSubsAcc

        genSubs : TyGen Substitutions
        genSubs =
            Set.foldl aggregate (TyGen.wrap Dict.empty) tvars
    in
    TyGen.map (\subs -> refine_type subs t) genSubs


env_get : String -> Env -> TyGen (Res Type)
env_get v e =
    case Dict.get v e of
        Just ( t, tvars ) ->
            instantiate_type t tvars
                |> TyGen.map Ok

        Nothing ->
            ("unbound variable: " ++ v)
                |> Err
                |> TyGen.wrap


refine_env : Substitutions -> Env -> Env
refine_env s env =
    let
        refine_entry _ ( t, v ) =
            ( refine_type (Set.foldl Dict.remove s v) t
            , v
            )
    in
    Dict.map refine_entry env


unify : Type -> Type -> Substitutions -> Res Substitutions
unify t1 t2 s =
    let
        t1_refined =
            refine_type s t1

        t2_refined =
            refine_type s t2

        cycle v t =
            Set.member v (tyvars_from_type t)
    in
    case ( t1_refined, t2_refined ) of
        ( CA.TypeConstant c1, CA.TypeConstant c2 ) ->
            if c1.name == c2.name then
                Ok s

            else
                Err <| "cannot unify " ++ c1.name ++ " and " ++ c2.name

        ( CA.TypeVariable v1, CA.TypeVariable v2 ) ->
            if v1 == v2 then
                Ok s

            else
                s
                    |> Dict.insert v1.name t2_refined
                    |> Ok

        ( CA.TypeVariable v1, _ ) ->
            if cycle v1.name t2 then
                -- is this the correct behavior?
                Err "cycle!"

            else
                s
                    |> Dict.insert v1.name t2_refined
                    |> Ok

        ( _, CA.TypeVariable v2 ) ->
            unify t2_refined t1_refined s

        ( CA.TypeFunction a, CA.TypeFunction b ) ->
            s
                |> unify a.to b.to
                |> Result.andThen (unify a.from b.from)

        _ ->
            Err <| "cannot unify " ++ Debug.toString t1_refined ++ " and " ++ Debug.toString t2_refined


literalToType : literal -> Type
literalToType l =
    -- TODO
    CA.TypeConstant { name = "Number" }


generalize : String -> Env -> Type -> ( Type, Set String )
generalize name env ty =
    let
        fold k ( t, tvars ) acc =
            -- don't add the value's own tyvars!
            if k == name then
                acc

            else
                Set.union (Set.diff (tyvars_from_type t) tvars) acc

        tyvarsFromEnv : Set String
        tyvarsFromEnv =
            Dict.foldl fold Set.empty env

        d =
            Set.diff (tyvars_from_type ty) tyvarsFromEnv
    in
    ( ty, d )


inspect_expr : Env -> CA.Expression e -> Type -> Substitutions -> TyGen (Res Substitutions)
inspect_expr env expr ty subs =
    case expr of
        CA.NumberLiteral _ l ->
            subs
                |> unify ty (literalToType l)
                |> TyGen.wrap

        CA.Variable _ { name } ->
            -- Every time I use a var with variable type, it should be instantiated,
            -- because each time it may by used against a different type.
            -- This is done automatically by `env_get`.
            do_nr (env_get name env) <| \nt ->
            let
                t =
                    refine_type subs nt
            in
            subs
                |> unify ty t
                |> TyGen.wrap

        CA.Lambda _ args ->
            TyGen.do newType <| \v_t ->
            TyGen.do newType <| \e_t ->
            do_nr (unify ty (CA.TypeFunction { from = v_t, fromIsMutable = False, to = e_t }) subs |> TyGen.wrap) <| \subs1 ->
            let
                v =
                    args.parameter

                e =
                    args.body

                env1 =
                    env_add v ( v_t, Set.empty ) env
            in
            do_nr (inspectStatementList e env1 subs1) <| \( returnType, _, subs2 ) ->
            unify e_t returnType subs2
                |> TyGen.wrap

        CA.Call _ args ->
            let
                f =
                    args.reference

                e =
                    args.argument
            in
            TyGen.do newType <| \e_t ->
            do_nr (inspect_expr env e e_t subs) <| \e_subs ->
            let
                f_t =
                    CA.TypeFunction { from = e_t, fromIsMutable = False, to = ty }

                f_t1 =
                    refine_type e_subs f_t
            in
            inspect_expr (refine_env e_subs env) f f_t1 e_subs

        CA.If _ _ ->
            ("inference NI: " ++ Debug.toString expr)
                |> Err
                |> TyGen.wrap

        CA.Record _ attrs ->
            let
                init =
                    ( [], subs )
                        |> Ok
                        |> TyGen.wrap

                foldAttr :
                    { name : String, value : CA.Expression e }
                    -> TyGen (Res ( List { name : String, type_ : Type }, Substitutions ))
                    -> TyGen (Res ( List { name : String, type_ : Type }, Substitutions ))
                foldAttr attr genResAccum =
                    do_nr genResAccum <| \( attrsAccum, subsAccum ) ->
                    TyGen.do newType <| \nt ->
                    do_nr (inspect_expr env attr.value nt subsAccum) <| \newSubs ->
                    ( { name = attr.name, type_ = nt } :: attrsAccum, newSubs )
                        |> Ok
                        |> TyGen.wrap
            in
            do_nr (List.foldl foldAttr init attrs) <| \( attrTypes, newSubs ) ->
            let
                refinedAttrTypes =
                    -- first I need all new subs, only then it makes sense to apply them
                    List.map (\a -> { a | type_ = refine_type newSubs a.type_ }) attrTypes
            in
            newSubs
                |> unify ty (CA.TypeRecord refinedAttrTypes)
                |> TyGen.wrap


inferExpr nextId preamble expr =
    let
        env =
            Dict.map (\n t -> ( t, Set.empty )) preamble

        genResType =
            TyGen.do newType <| \nt ->
            do_nr (inspect_expr env expr nt Dict.empty) <| \subs ->
            refine_type subs nt
                |> Ok
                |> TyGen.wrap

        ( res, nextId1 ) =
            TyGen.run nextId genResType
    in
    res
        |> Result.map (\type_ -> ( type_, Dict.empty, nextId1 ))



----
---
--


inspect_statement : CA.Statement e -> Env -> Substitutions -> TyGen (Res ( Type, Env, Substitutions ))
inspect_statement statement env subs =
    case statement of
        CA.Evaluation expr ->
            TyGen.do newType <| \nt ->
            do_nr (inspect_expr env expr nt subs) <| \subs1 ->
            let
                refinedNt =
                    refine_type subs1 nt
            in
            ( refinedNt, env, subs1 )
                |> Ok
                |> TyGen.wrap

        CA.Assignment _ ->
            "Assignment not implemented"
                |> Err
                |> TyGen.wrap

        CA.Definition { name, body, maybeAnnotation } ->
            do_nr (inspectStatementList body env subs) <| \( returnType, _, subs1 ) ->
            TyGen.wrap
                (case Dict.get name env of
                    Nothing ->
                        Debug.todo "WTF dict should contain def name already"

                    Just ( defType, defFreeTypeVars ) ->
                        unify returnType defType subs1
                            |> Result.map
                                (\subs2 ->
                                    let
                                        refinedType =
                                            refine_type subs2 defType

                                        scheme =
                                            -- this gets all "forall" vars in the definition
                                            generalize name (refine_env subs2 env) refinedType

                                        env1 =
                                            env_add name scheme env
                                    in
                                    -- A definition has no type
                                    ( typeNone, env1, subs2 )
                                )
                )



----
--- Definition Body
--


inspectStatementList : List (CA.Statement e) -> Env -> Substitutions -> TyGen (Res ( Type, Env, Substitutions ))
inspectStatementList stats parentEnv subs =
    let
        -- A statement list can contain definitions, creating its own scope
        -- Definitions can be recursive and in general appear in any order, so we want to add them to the environment before we inspect them
        definitions =
            List.filterMap statementAsDefinition stats
    in
    do_nr (insertDefinitionRec definitions parentEnv) <| \localEnv ->
    inspectStatementRec stats typeNone localEnv subs


inspectStatementRec : List (CA.Statement e) -> Type -> Env -> Substitutions -> TyGen (Res ( Type, Env, Substitutions ))
inspectStatementRec stats returnType env subs =
    case stats of
        [] ->
            ( returnType, env, subs )
                |> Ok
                |> TyGen.wrap

        stat :: statsTail ->
            do_nr (inspect_statement stat env subs) <| \( ty, env1, subs1 ) ->
            inspectStatementRec statsTail ty env1 subs1


insertDefinitionRec : List (CA.ValueDefinition e) -> Env -> TyGen (Res Env)
insertDefinitionRec defs env =
    case defs of
        [] ->
            env
                |> Ok
                |> TyGen.wrap

        def :: ds ->
            if Dict.member def.name env then
                (def.name ++ " already declared in scope!")
                    |> Err
                    |> TyGen.wrap

            else
                TyGen.do newName <| \name ->
                env
                    |> env_add def.name ( CA.TypeVariable { name = name }, Set.singleton name )
                    |> insertDefinitionRec ds


statementAsDefinition : CA.Statement e -> Maybe (CA.ValueDefinition e)
statementAsDefinition stat =
    case stat of
        CA.Definition d ->
            Just d

        _ ->
            Nothing
