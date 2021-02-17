module Main exposing (..)

import Array exposing (Array)
import Browser
import Compiler.ApplyAliases
import Compiler.ApplyAliases_Test
import Compiler.CanonicalToJs
import Compiler.FindUndeclared
import Compiler.FindUndeclared_Test
import Compiler.FormattableToCanonicalAst
import Compiler.FormattableToCanonicalAst_Test
import Compiler.JsToString
import Compiler.JsToString_Test
import Compiler.StringToTokens
import Compiler.StringToTokens_Test
import Compiler.TestHelpers
import Compiler.TokensToFormattableAst
import Compiler.TokensToFormattableAst_Test
import Compiler.TypeInference
import Compiler.TypeInference_Test exposing (preamble)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events
import Markdown
import OneOrMore exposing (OneOrMore)
import Parser
import Set exposing (Set)
import Test
import Types.CanonicalAst as CA
import Types.Error exposing (Res)
import Types.FormattableAst as FA
import Types.Token


runTests =
    False
        || True


initialCode =
    """

record = { x = { y = { z = 4 } } }

remm =
   m @= record

result = remm

    """


allCode =
    """
[#
   SquarePants has no import statements: instead, project-wide imports are declared in modules.toml
#]



# Basic stuff

numberOne =
  1

addThreeNumbers x y z =
  x + y + z

floatOne : Float
floatOne =
  1

fibonacci : Int -> Int
fibonacci n =
  if n < 2 then n else n + fibonacci (n - 1)

listOfText : [ Text ]
listOfText = [
  , "Gary"
  , "Karen"
  , "I'm ready! Promotion!"
  ]



# Mutability

average : List Int -> Float
average numbers =
  n @= 0
  sum @= 0

  List.each fn x =
    @n += 1
    @sum += x

  # division by 0 yields 0
  sum / n



# Algebraic Data Types

union LoadingState payload =
    , NotRequested
    , Requested
    , Error Text
    , Available payload

getStatusName : LoadingState payload -> Text
getStatusName loadingState =
  try LoadingState as
    NotRequested then "Not needed"
    Requested then "Awaiting server response"
    Error message then "Error: " .. message
    Available _ then "Successfully loaded"

getPayload : LoadingState payload -> Maybe payload
getPayload loadingState =
  try loadingState as Available payload then Just payload else Nothing



# Records

alias Crab = {
  , name : Text
  , money : Float
  }

eugeneKrabs : Crab
eugeneKrabs = {
  , name = "Eugene H. Krabs"
  , money = 2_345_678.90
  }


# TODO record access example


earnMoney : Float -> Crab -> Crab
earnMoney profit crab =
  { crab with money = .money + profit }




# Tuples TODO




    """


tests =
    if not runTests then
        Html.text ""

    else
        Test.viewList
            [ Compiler.StringToTokens_Test.tests
            , Compiler.TokensToFormattableAst_Test.tests
            , Compiler.FormattableToCanonicalAst_Test.tests
            , Compiler.TypeInference_Test.tests
            , Compiler.FindUndeclared_Test.tests
            , Compiler.ApplyAliases_Test.tests
            , Compiler.JsToString_Test.tests
            ]


type alias Model =
    { code : String
    }


type Msg
    = OnInput String


init : Model
init =
    Model initialCode


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnInput code ->
            { model | code = code }


view : Model -> Html Msg
view model =
    let
        code =
            model.code

        tokens =
            code
                |> Compiler.StringToTokens.lexer

        faModule =
            tokens
                |> Result.andThen Compiler.TokensToFormattableAst.parse

        caModule =
            faModule
                |> Result.andThen Compiler.FormattableToCanonicalAst.translateModule

        undeclared =
            caModule
                |> Result.mapError (always Dict.empty)
                |> Result.andThen Compiler.FindUndeclared.moduleUndeclared

        --
        -- missing step: load undeclared from other modules
        --
        alModule =
            caModule
                |> Result.andThen Compiler.ApplyAliases.applyAliasesToModule

        inference =
            alModule
                |> Result.andThen (Compiler.TypeInference.inspectModule preamble)

        js =
            caModule
                |> Result.andThen emitModule

        --
        --
        onOk : (a -> Html msg) -> Res a -> Html msg
        onOk f res =
            case res of
                Ok a ->
                    f a

                Err e ->
                    Html.code
                        []
                        [ e
                            |> Compiler.TestHelpers.errorToString code
                            |> Html.text
                        ]
    in
    Html.div
        [ style "display" "flex"

        --         , style "background-color" "black"
        --         , style "color" "white"
        ]
        [ Html.textarea
            [ style "width" "50%"
            , style "min-width" "30%"
            , style "height" "99vh"

            --             , style "background-color" "black"
            --             , style "color" "white"
            , Html.Events.onInput OnInput
            ]
            [ Html.text model.code ]
        , Html.ul
            []
            [ Html.li
                []
                [ Html.h6 [] [ Html.text "Run $result" ]
                , viewEval model.code
                ]
            , Html.li
                []
                [ Html.h6 [] [ Html.text "Emit" ]
                , onOk viewJs js
                ]
            , Html.li
                []
                [ Html.h6 [] [ Html.text "Inference" ]
                , onOk viewInference inference
                ]
            , Html.li
                []
                [ Html.h6 [] [ Html.text "Alias expansion" ]
                , onOk viewCanonicalAst alModule
                ]
            , Html.li
                []
                [ Html.h6 [] [ Html.text "Undeclared" ]
                , viewUndeclared undeclared
                ]
            , Html.li
                []
                [ Html.h6 [] [ Html.text "Canonical AST" ]
                , onOk viewCanonicalAst caModule
                ]
            , Html.li
                []
                [ Html.h6 [] [ Html.text "Formattable AST" ]
                , onOk viewFormattableAst faModule
                ]
            , Html.li
                []
                [ Html.h6 [] [ Html.text "Tokens" ]
                , onOk viewTokens tokens
                ]
            , if not runTests then
                Html.li
                    []
                    [ Html.h6
                        [ style "color" "red" ]
                        [ Html.text "TESTS DISABLED" ]
                    ]

              else
                Html.li
                    []
                    [ Html.h6 [] [ Html.text "Tests" ]
                    , tests
                    ]
            ]
        ]



----
--- JS
--


viewEval : String -> Html msg
viewEval code =
    Html.code
        []
        [ Html.pre
            []
            [ case Compiler.JsToString_Test.eval "result" code of
                Ok res ->
                    Html.text res

                Err message ->
                    Html.text <| "Error: ### " ++ message ++ " ###"
            ]
        ]


viewJs : String -> Html msg
viewJs js =
    Html.code
        []
        [ Html.pre
            []
            [ Html.text js ]
        ]


emitModule : CA.Module e -> Result x String
emitModule caModule =
    [ caModule ]
        |> Compiler.CanonicalToJs.translateAll
        |> List.map (Compiler.JsToString.emitStatement 0)
        |> (++) [ Compiler.CanonicalToJs.cloneDefinition ]
        |> String.join "\n\n"
        |> Ok



----
--- Inference
--
-- viewInference : (Compiler.TypeInference.Eas -> Html msg


viewInference ( mod, env, subs ) =
    Html.div
        []
        [ env
            |> Dict.toList
            |> List.filter (\( k, v ) -> not (Dict.member k preamble))
            |> List.map (\( k, v ) -> Html.div [] [ k ++ ": " ++ viewSchema v |> Html.text ])
            |> Html.div []

        {-
           , mod.values
               |> Dict.values
               --             |> List.sortBy .name
               |> List.map viewCaDefinition
               |> Html.code []
        -}
        , subs
            |> Dict.toList
            |> List.map (\( k, v ) -> Html.div [] [ k ++ " => " ++ viewCaType v |> Html.text ])
            |> Html.div []
        ]


viewSchema : Compiler.TypeInference.EnvEntry -> String
viewSchema schema =
    [ "forall: [" ++ String.join "," (Set.toList schema.forall) ++ "]"
    , "mutable: " ++ Debug.toString schema.mutable
    , "type: " ++ viewCaType schema.type_
    ]
        |> String.join " ### "



----
--- Undeclared
--


viewUndeclared : Result Compiler.FindUndeclared.Undeclared Compiler.FindUndeclared.EnvUndeclared -> Html msg
viewUndeclared un =
    case un of
        Err undeclaredTypeVars ->
            Html.div
                []
                [ Html.text <| "UNDECLARED TYPE VARS: " ++ Debug.toString undeclaredTypeVars ]

        Ok m ->
            Html.div
                []
                [ Html.div [] [ Html.text <| "types: " ++ Debug.toString m.types ]
                , Html.div [] [ Html.text <| "values: " ++ Debug.toString m.values ]
                ]



----
--- Canonical AST
--


viewCanonicalAst : CA.Module e -> Html msg
viewCanonicalAst mod =
    Html.div
        []
        [ mod.aliases
            |> Dict.values
            |> List.sortBy .name
            |> List.map viewCaAlias
            |> Html.code []
        , mod.unions
            |> Dict.values
            |> List.sortBy .name
            |> List.map viewCaUnion
            |> Html.code []
        , mod.values
            |> Dict.values
            --             |> List.sortBy .name
            |> List.map viewCaDefinition
            |> Html.code []
        ]


viewCaAlias : CA.AliasDef -> Html msg
viewCaAlias al =
    Html.div
        []
        [ [ "alias:"
          , al.name
          , String.join " " al.args
          , "="
          , viewCaType al.ty
          ]
            |> String.join " "
            |> Html.text
        ]


viewCaUnion : CA.UnionDef -> Html msg
viewCaUnion u =
    Html.div
        []
        [ [ "union:"
          , u.name
          , String.join " " u.args
          , "="
          , Debug.toString u.constructors
          ]
            |> String.join " "
            |> Html.text
        ]


viewCaDefinition : CA.ValueDef e -> Html msg
viewCaDefinition def =
    Html.div
        []
        [ Html.div
            []
            [ def.maybeAnnotation
                |> Maybe.map (\x -> viewCaPattern def.pattern ++ " : " ++ viewCaType x)
                |> Maybe.withDefault ""
                |> Html.text
            ]
        , Html.div
            []
            [ Html.text <| viewCaPattern def.pattern ++ " = " ]
        , Html.div
            [ style "padding-left" "2em" ]
            (List.map viewCaStatement def.body)
        ]


viewCaPattern : CA.Pattern -> String
viewCaPattern p =
    case p of
        CA.PatternDiscard ->
            "_"

        CA.PatternAny n ->
            n

        _ ->
            Debug.toString p


viewCaType : CA.Type -> String
viewCaType ty =
    case ty of
        CA.TypeConstant { path, args } ->
            path ++ " " ++ String.join " " (List.map viewCaType args)

        CA.TypeVariable { name } ->
            name

        CA.TypeAlias path t ->
            "<" ++ path ++ ": " ++ viewCaType t ++ ">"

        CA.TypeFunction { from, fromIsMutable, to } ->
            [ "(" ++ viewCaType from ++ ")"
            , case fromIsMutable of
                Just True ->
                    " @> "

                Just False ->
                    " -> "

                Nothing ->
                    " ?> "
            , "(" ++ viewCaType to ++ ")"
            ]
                |> String.join ""

        CA.TypeRecord args ->
            let
                var =
                    case args.extensible of
                        Just name ->
                            name ++ " with "

                        Nothing ->
                            ""
            in
            args.attrs
                |> Dict.toList
                |> List.sortBy Tuple.first
                |> List.map (\( name, type_ ) -> name ++ ": " ++ viewCaType type_)
                |> String.join ", "
                |> (\s -> "{" ++ var ++ s ++ "}")


viewCaStatement : CA.Statement e -> Html msg
viewCaStatement s =
    case s of
        CA.Evaluation expr ->
            Html.div
                []
                [ Html.text "Evaluation: "
                , viewCaExpression expr
                ]

        CA.Definition def ->
            viewCaDefinition def


viewCaExpression : CA.Expression e -> Html msg
viewCaExpression expr =
    case expr of
        CA.Literal _ s ->
            Html.text (Debug.toString s)

        CA.Variable x s ->
            Html.text <| Debug.toString x ++ "|" ++ s.path

        CA.Call _ { reference, argument } ->
            Html.div
                [ style "border" "red" ]
                [ viewCaExpression reference
                , Html.div
                    [ style "padding-left" "2em" ]
                    [ case argument of
                        CA.ArgumentMutable args ->
                            Html.text <| "@" ++ args.path ++ String.join ":" args.attrPath

                        CA.ArgumentExpression e ->
                            viewCaExpression e
                    ]
                ]

        _ ->
            Html.code
                []
                [ expr
                    |> Debug.toString
                    |> Html.text
                ]



----
--- Formattable AST
--


viewFormattableAst : FA.Module -> Html msg
viewFormattableAst statements =
    statements
        |> List.map viewFaStatement
        |> Html.div []


viewFaStatement : FA.Statement -> Html msg
viewFaStatement s =
    case s of
        FA.UnionDef td ->
            Html.div
                []
                [ td
                    |> Debug.toString
                    |> (++) "type definition: "
                    |> Html.text
                ]

        FA.TypeAlias td ->
            Html.div
                []
                [ td
                    |> Debug.toString
                    |> (++) "type alias: "
                    |> Html.text
                ]

        FA.Evaluation expr ->
            Html.div
                []
                [ Html.text "Evaluation: "
                , viewFaExpression expr
                ]

        FA.Definition { pattern, maybeAnnotation, body } ->
            Html.div
                []
                [ Html.span
                    []
                    [ Html.text (viewFaPattern pattern)
                    ]
                , maybeAnnotation
                    |> Maybe.map (\x -> " : " ++ Debug.toString x)
                    |> Maybe.withDefault ""
                    |> Html.text
                , body
                    |> OneOrMore.toList
                    |> List.map viewFaStatement
                    |> Html.div []
                ]


viewFaPattern : FA.Pattern -> String
viewFaPattern p =
    case p of
        FA.PatternAny n ->
            n

        _ ->
            Debug.toString p


viewFaExpression : FA.Expression -> Html msg
viewFaExpression expr =
    case expr of
        FA.Literal s ->
            Html.text (Debug.toString s)

        FA.Variable s ->
            Html.text s.name

        FA.FunctionCall { reference, arguments } ->
            Html.div
                [ style "border" "red" ]
                [ Html.div
                    []
                    [ Html.text "[call] "
                    , viewFaExpression reference
                    ]
                , Html.div
                    [ style "padding-left" "2em" ]
                    (List.map viewFaExpression <| Tuple.first arguments :: Tuple.second arguments)
                ]

        FA.Binop { group, sepList } ->
            Html.div
                [ style "border" "red" ]
                [ Html.div
                    []
                    [ Html.text <| "[op] " ]

                --TODO ++ op ]
                , Html.div
                    [ style "padding-left" "2em" ]
                    []

                --TODO viewFaExpression left ]
                , Html.div
                    []
                    [ Html.text "---" ]
                , Html.div
                    [ style "padding-left" "2em" ]
                    []

                --TODO viewFaExpression right ]
                ]

        --     Unop String Expression
        _ ->
            Html.code
                []
                [ expr
                    |> Debug.toString
                    |> Html.text
                ]



----
--- Tokens
--


viewTokens : List Types.Token.Token -> Html msg
viewTokens tokens =
    tokens
        |> List.map (\t -> Html.div [] [ Html.text (Debug.toString t.kind) ])
        |> Html.div []



----
---
--


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
