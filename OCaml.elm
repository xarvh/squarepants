module Main exposing (..)

{-| This is a translation of [this OCaml implementation](https://pfudke.wordpress.com/2014/11/20/hindley-milner-type-inference-a-practical-example-2/)
-}

import Browser
import Dict exposing (Dict)
import Generator as Next
import Html
import Set exposing (Set)



----
--- Variable type generator
--


type alias Next a =
    Next.Generator Int a


newType : Next Ty
newType =
    Next.next ((+) 1) (\n -> TyVar <| "t" ++ String.fromInt n)



----
---
--


type alias Res a =
    Result String a



----
---
--


type Ty_literal
    = TyInt
    | TyBool
    | TyString


type Ty
    = TyVar String
    | TyLit Ty_literal
    | TyLambda Ty Ty


type Literal
    = Int Int
    | Bool Bool
    | String String


type Expr
    = Lit Literal
    | Var String
    | Let String Expr Expr
    | Lambda String Expr
    | App Expr Expr


ty_repr : Ty -> String
ty_repr ty =
    case ty of
        TyVar v ->
            v

        TyLit l ->
            case l of
                TyInt ->
                    "int"

                TyBool ->
                    "bool"

                TyString ->
                    "string"

        TyLambda f c ->
            (case f of
                TyLambda _ _ ->
                    "(" ++ ty_repr f ++ ")"

                _ ->
                    ty_repr f
            )
                ++ " -> "
                ++ (case c of
                        TyLambda _ _ ->
                            "(" ++ ty_repr c ++ ")"

                        _ ->
                            ty_repr c
                   )


expr_repr : Expr -> String
expr_repr expr =
    case expr of
        Lit l ->
            case l of
                Int i ->
                    String.fromInt i

                Bool True ->
                    "true"

                Bool False ->
                    "false"

                String s ->
                    "\"" ++ s ++ "\""

        Var v ->
            v

        Let v e1 e2 ->
            "let "
                ++ v
                ++ " = "
                ++ expr_repr e1
                ++ " in "
                ++ expr_repr e2

        Lambda v e ->
            "fun " ++ v ++ " -> " ++ expr_repr e

        App f e ->
            expr_repr f
                ++ " ("
                ++ expr_repr e
                ++ ")"



----
--- Substs
--


type alias Subst =
    Dict String Ty


make_constraint =
    Dict.insert


refine_type : Subst -> Ty -> Ty
refine_type s t =
    case t of
        TyVar v ->
            case Dict.get v s of
                Just substitutionType ->
                    {- a substitution exists for the variable type v -}
                    refine_type s substitutionType

                Nothing ->
                    {- no substitution, return the type as-is -}
                    TyVar v

        TyLambda a b ->
            TyLambda (refine_type s a) (refine_type s b)

        TyLit l ->
            TyLit l


tyvars_from_type : Ty -> Set String
tyvars_from_type ty =
    case ty of
        TyVar v ->
            Set.singleton v

        TyLambda a b ->
            Set.union (tyvars_from_type a) (tyvars_from_type b)

        TyLit l ->
            Set.empty



----
--- Env
--


type alias Env =
    -- (Ty, Set String) is a type's "schema"?
    Dict String ( Ty, Set String )


tyvars_from_env : Env -> Set String
tyvars_from_env e =
    Dict.foldl
        (\k ( t, tvars ) acc -> Set.union (Set.diff (tyvars_from_type t) tvars) acc)
        Set.empty
        e


{-| string -> ty -> env -> env
-}
env_add : String -> ( Ty, Set String ) -> Env -> Env
env_add v sc e =
    Dict.insert v sc e


instantiate_type : Ty -> Set String -> Next Ty
instantiate_type t tvars =
    Next.do newType <| \nt ->
    let
        aggregate =
            \v acc -> Dict.insert v nt acc

        subs =
            Set.foldl aggregate Dict.empty tvars
    in
    refine_type subs t
        |> Next.wrap


env_get : String -> Env -> Next Ty
env_get v e =
    case Dict.get v e of
        Just ( t, tvars ) ->
            instantiate_type t tvars

        Nothing ->
            Debug.todo "(Unbound_variable v)"


refine_env : Subst -> Env -> Env
refine_env s env =
    let
        refine_entry _ ( t, v ) =
            ( refine_type (Set.foldl Dict.remove s v) t
            , v
            )
    in
    Dict.map refine_entry env


unify : Ty -> Ty -> Subst -> Res Subst
unify t1 t2 s =
    let
        t1_refined =
            refine_type s t1

        t2_refined =
            refine_type s t2

        cycle v t =
            Set.member v (tyvars_from_type t)
    in
    if t1_refined == t2_refined then
        Ok s

    else
        case ( t1_refined, t2_refined ) of
            ( TyVar v1, _ ) ->
                if cycle v1 t2 then
                    -- TODO is this the correct behavior?
                    Err "cycle!"

                else
                    make_constraint v1 t2_refined s
                        |> Ok

            ( _, TyVar v2 ) ->
                unify t2_refined t1_refined s

            ( TyLambda from1 to1, TyLambda from2 to2 ) ->
                s
                    |> unify to1 to2
                    |> Result.andThen (unify from1 from2)

            _ ->
                Err "(Cannot_unify (a, b))"


lit_to_type : Literal -> Ty_literal
lit_to_type l =
    case l of
        Int _ ->
            TyInt

        Bool _ ->
            TyBool

        String _ ->
            TyString


generalize : Env -> Ty -> ( Ty, Set String )
generalize e t =
    let
        tvars =
            tyvars_from_type t

        etvars =
            tyvars_from_env e

        d =
            Set.diff tvars etvars
    in
    ( t, d )


do_nr : Next (Res a) -> (a -> Next (Res b)) -> Next (Res b)
do_nr nra f =
    Next.do nra
        (\ra ->
            case ra of
                Ok a ->
                    f a

                Err e ->
                    Next.wrap (Err e)
        )


inspect_expr : Env -> Expr -> Ty -> Subst -> Next (Res Subst)
inspect_expr env expr ty subs =
    case expr of
        Lit l ->
            subs
                |> unify ty (TyLit (lit_to_type l))
                |> Next.wrap

        Var v ->
            Next.do (env_get v env) <| \stuff ->
            let
                t =
                    refine_type subs stuff
            in
            unify ty t subs
                |> Next.wrap

        Let v e1 e2 ->
            Next.do newType <| \e1_ty ->
            do_nr (inspect_expr env e1 e1_ty subs) <| \subs1 ->
            let
                e1_ty1 =
                    refine_type subs1 e1_ty

                scheme =
                    generalize (refine_env subs1 env) e1_ty1

                env1 =
                    env_add v scheme env
            in
            inspect_expr (refine_env subs1 env1) e2 ty subs1

        Lambda v e ->
            Next.do newType <| \v_t ->
            Next.do newType <| \e_t ->
            do_nr (unify ty (TyLambda v_t e_t) subs |> Next.wrap) <| \new_subs ->
            let
                new_env =
                    env_add v ( v_t, Set.empty ) env
            in
            inspect_expr new_env e e_t new_subs

        App f e ->
            Next.do newType <| \e_t ->
            do_nr (inspect_expr env e e_t subs) <| \e_subs ->
            let
                f_t =
                    TyLambda e_t ty

                f_t1 =
                    refine_type e_subs f_t
            in
            inspect_expr (refine_env e_subs env) f f_t1 e_subs


default_env : Env
default_env =
    [ ( "+"
      , TyLambda
            (TyLit TyInt)
            (TyLambda
                (TyLit TyInt)
                (TyLit TyInt)
            )
      )
    , ( "-"
      , TyLambda
            (TyLit TyInt)
            (TyLambda
                (TyLit TyInt)
                (TyLit TyInt)
            )
      )
    , ( "^"
      , TyLambda
            (TyLit TyString)
            (TyLambda
                (TyLit TyString)
                (TyLit TyString)
            )
      )
    , ( "="
      , TyLambda
            (TyVar "a")
            (TyLambda
                (TyVar "a")
                (TyLit TyBool)
            )
      )
    , ( "not"
      , TyLambda
            (TyLit TyBool)
            (TyLit TyBool)
      )
    ]
        |> List.map (Tuple.mapSecond (\ty -> ( ty, Set.empty )))
        |> Dict.fromList


type_of : Expr -> Res Ty
type_of e =
    let
        env =
            default_env

        empty_subs =
            Dict.empty

        nn =
            Next.do newType <| \e_t ->
            do_nr (inspect_expr env e e_t empty_subs) <| \xxxx ->
            refine_type xxxx e_t
                |> Ok
                |> Next.wrap
    in
    Next.run 0 nn
        |> Tuple.first



----
---
--


exprs =
    [ App
        (App (Var "+") (Lit (Int 42)))
        (Lit (Int 1))
    , App
        (Var "not")
        (App
            (App
                (Var "=")
                (Lit (String "foo"))
            )
            (Lit (String "bar"))
        )
    , Lambda "x" (Var "x")
    , Let
        "id"
        (Lambda
            "x"
            (Let "y" (Var "x") (Var "y"))
        )
        (App (Var "id") (Var "id"))
    , Let
        "id"
        (Lambda
            "x"
            (Let "y" (Var "x") (Var "y"))
        )
        (App
            (App (Var "id") (Var "id"))
            (Lit (Int 42))
        )
    , Lambda
        "m"
        (Let
            "y"
            (Var "m")
            (Let
                "x"
                (App (Var "y") (Lit (Bool True)))
                (Var "x")
            )
        )
    ]


viewExpr expr =
    Html.li
        []
        [ Html.div [] [ Html.text (expr_repr expr) ]
        , Html.div []
            [ case type_of expr of
                Ok typeOfE ->
                    Html.text (ty_repr typeOfE)

                Err e ->
                    Html.text e
            ]
        ]


main =
    Browser.sandbox
        { init = ""
        , update = \msg model -> model
        , view =
            \model ->
                exprs
                    |> List.map viewExpr
                    |> Html.ul []
        }
