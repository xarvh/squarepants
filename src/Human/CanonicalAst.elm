module Human.CanonicalAst exposing (..)

import Dict exposing (Dict)
import StateMonad as M exposing (M, do, get, return)
import Types.CanonicalAst as CA exposing (Type)


typeToString : Type -> String
typeToString =
    typeToPriAndString >> Tuple.second


parensIf : Bool -> String -> String
parensIf test s =
    if test then
        "(" ++ s ++ ")"

    else
        s


parensIfGreaterThan : Int -> Type -> String
parensIfGreaterThan threshold ty =
    let
        ( pri, str ) =
            typeToPriAndString ty
    in
    parensIf (pri > threshold) str


typeToPriAndString : CA.Type -> ( Int, String )
typeToPriAndString type_ =
    case type_ of
        CA.TypeConstant pos name args ->
            ( if args == [] then
                0

              else
                1
            , name
                :: List.map (parensIfGreaterThan 0) args
                |> String.join " "
            )

        CA.TypeVariable pos name ->
            ( 0
            , name
            )

        CA.TypeFunction pos from fromIsMut to ->
            let
                arrow =
                    if fromIsMut then
                        " @: "

                    else
                        ": "
            in
            ( 2
            , [ parensIfGreaterThan 1 from
              , arrow
              , parensIfGreaterThan 2 to
              ]
                |> String.join ""
            )

        CA.TypeRecord pos extend attrs ->
            let
                attrsString =
                    attrs
                        |> Dict.toList
                        |> List.sortBy Tuple.first
                        |> List.map (\( name, ty ) -> name ++ " as " ++ typeToString ty)
                        |> String.join ", "
            in
            ( 0
            , [ "{"
              , case extend of
                    Nothing ->
                        ""

                    Just n ->
                        n ++ " with"
              , attrsString
              , "}"
              ]
                |> String.join " "
            )

        CA.TypeAlias pos name ty2 ->
            ( 0
              {-

                 , [ "<"
                   , name
                   , "="
                   , typeToString ty2
                   , ">"
                   ]
                     |> String.join " "
              -}
            , name
            )



----
--- Humanize generated tvar names, ie "2" -> "a"
--


type alias NormMonad a =
    M NormState a


type alias NormState =
    { replacements : Dict String String
    , next : Int
    }


initNstate : NormState
initNstate =
    { replacements = Dict.empty
    , next = 0
    }


intToName : Int -> List Char -> String
intToName n acc =
    --
    -- TODO this is wrong, because sometimes `a` is used as 0 sometimes as 1
    --
    -- works: 26 * 26 -> "aaa"
    -- doesnt: 26 * 26 - 1 -> "zz"
    --
    -- Don't want to think about this right now
    --
    let
        zero =
            Char.toCode 'a'

        nine =
            Char.toCode 'z'

        base =
            nine - zero + 1

        head =
            modBy base n + zero

        r =
            n // base
    in
    if r == 0 then
        -- `a` is zero, so the most significant symbol will start at `b` unless we remove 1
        -- (per above, this is wrong)
        if acc == [] then
            String.fromList [ Char.fromCode head ]

        else
            String.fromList (Char.fromCode (head - 1) :: acc)

    else
        intToName r (Char.fromCode head :: acc)


newName : NormMonad String
newName state =
    ( intToName state.next []
    , { state | next = state.next + 1 }
    )


normalizeType : Type -> Type
normalizeType =
    normType >> M.run initNstate >> Tuple.first


normalizeTypeAndTyvars : Type -> Dict String a -> ( Type, Dict String a )
normalizeTypeAndTyvars tyOld tyvarsOld =
    let
        ( tyNew, state ) =
            tyOld
                |> normType
                |> M.run initNstate

        replace name =
            Dict.get name state.replacements
                |> Maybe.withDefault name
                |> Dict.insert

        tyvarsNew =
            Dict.foldl replace Dict.empty tyvarsOld
    in
    ( tyNew, tyvarsNew )


normType : Type -> NormMonad Type
normType ty =
    case ty of
        CA.TypeConstant pos name args ->
            do (M.list_map normType args) <| \args_n ->
            return <| CA.TypeConstant pos name args_n

        CA.TypeVariable pos name ->
            do (normName name) <| \n ->
            return <| CA.TypeVariable pos n

        CA.TypeFunction pos from0 fromIsMut to0 ->
            do (normType from0) <| \from1 ->
            do (normType to0) <| \to1 ->
            return <| CA.TypeFunction pos from1 fromIsMut to1

        CA.TypeRecord pos ext0 attrs0 ->
            do (M.maybe_map normName ext0) <| \ext1 ->
            do (M.dict_map (\k -> normType) attrs0) <| \attrs1 ->
            return <| CA.TypeRecord pos ext1 attrs1

        CA.TypeAlias pos path t ->
            do (normType t) <| \t1 ->
            return <| CA.TypeAlias pos path t1


normName : String -> NormMonad String
normName name =
    -- Replace only names that are numbers ("22") or single letters ("a")
    if String.length name > 1 && String.toInt name == Nothing then
        return name

    else
        do (get .replacements) <| \n2l ->
        case Dict.get name n2l of
            Just replacement ->
                return replacement

            Nothing ->
                do newName <| \n ->
                let
                    addReplacement : NormState -> NormState
                    addReplacement s =
                        { s | replacements = Dict.insert name n s.replacements }
                in
                do (M.update addReplacement) <| \_ ->
                return n
