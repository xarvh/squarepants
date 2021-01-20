module Main exposing (..)

import Array exposing (Array)
import Browser
import Compiler.FindUndeclared
import Compiler.FormattableToCanonicalAst
import Compiler.FormattableToCanonicalAst_Test
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
import OneOrMore exposing (OneOrMore)
import Parser
import Set exposing (Set)
import Test
import Types.CanonicalAst as CA
import Types.Error
import Types.FormattableAst as FA


runTests =
    False
        || True


initialCode =
    """
a : [ Bool ]
a = a
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
    Html.div
        [ style "display" "flex"

        --         , style "background-color" "black"
        --         , style "color" "white"
        ]
        [ Html.textarea
            [ style "width" "50%"
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
                [ Html.h6 [] [ Html.text "Inference" ]
                , viewInference model.code
                ]
            , Html.li
                []
                [ Html.h6 [] [ Html.text "Undeclared" ]
                , viewUndeclared model.code
                ]
            , Html.li
                []
                [ Html.h6 [] [ Html.text "Canonical AST" ]
                , viewCanonicalAst model.code
                ]
            , Html.li
                []
                [ Html.h6 [] [ Html.text "Formattable AST" ]
                , viewFormattableAst model.code
                ]
            , Html.li
                []
                [ Html.h6 [] [ Html.text "Tokens" ]
                , viewTokens model.code
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
--- Inference
--


viewInference : String -> Html msg
viewInference code =
    case Compiler.TestHelpers.stringToCanonicalModule code of
        Ok mod ->
            case Compiler.TypeInference.inspectModule preamble mod of
                Err err ->
                    Html.text err

                Ok env ->
                    env
                        |> Dict.toList
                        |> List.filter (\( k, v ) -> not (Dict.member k preamble))
                        |> List.map (\( k, v ) -> Html.div [] [ k ++ ": " ++ viewSchema v |> Html.text ])
                        |> Html.div []

        Err error ->
            error
                |> Debug.toString
                |> (++) "ERROR "
                |> Html.text


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


viewUndeclared : String -> Html msg
viewUndeclared code =
    case Compiler.TestHelpers.stringToCanonicalModule code of
        Err error ->
            error
                |> Debug.toString
                |> (++) "ERROR "
                |> Html.text

        Ok mod ->
            case Compiler.FindUndeclared.moduleUndeclared mod of
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


viewCanonicalAst : String -> Html msg
viewCanonicalAst code =
    let
        res =
            code
                |> Compiler.StringToTokens.lexer
                |> Result.andThen Compiler.TokensToFormattableAst.parse
                |> Result.andThen Compiler.FormattableToCanonicalAst.translateModule
    in
    case res of
        Ok mod ->
            Html.div
                []
                [ mod.aliases
                    |> Dict.values
                    |> List.sortBy .name
                    |> List.map viewCaAlias
                    |> Html.code []
                , mod.values
                    |> Dict.values
                    |> List.sortBy .name
                    |> List.map viewCaDefinition
                    |> Html.code []
                ]

        _ ->
            res
                |> Debug.toString
                |> Html.text


viewCaAlias : CA.Alias -> Html msg
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


viewCaDefinition : CA.ValueDefinition e -> Html msg
viewCaDefinition def =
    Html.div
        []
        [ Html.div
            []
            [ def.maybeAnnotation
                |> Maybe.map (\x -> def.name ++ " : " ++ viewCaType x)
                |> Maybe.withDefault ""
                |> Html.text
            ]
        , Html.div
            []
            [ Html.text <| def.name ++ " = " ]
        , Html.div
            [ style "padding-left" "2em" ]
            (List.map viewCaStatement def.body)
        ]


viewCaType : CA.Type -> String
viewCaType ty =
    case ty of
        CA.TypeConstant { path, args } ->
            path ++ " " ++ String.join " " (List.map viewCaType args)

        CA.TypeVariable { name } ->
            name

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
                            name ++ "|"

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
        CA.NumberLiteral _ s ->
            Html.text s.number

        CA.Variable _ s ->
            Html.text s.path

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


viewFormattableAst : String -> Html msg
viewFormattableAst code =
    let
        res =
            code
                |> Compiler.StringToTokens.lexer
                |> Result.andThen Compiler.TokensToFormattableAst.parse
    in
    case res of
        Ok statements ->
            statements
                |> List.map viewFaStatement
                |> Html.div []

        _ ->
            res
                |> Debug.toString
                |> Html.text


viewFaStatement : FA.Statement -> Html msg
viewFaStatement s =
    case s of
        FA.TypeDefinition td ->
            td
                |> Debug.toString
                |> (++) "type definition: "
                |> Html.text

        FA.TypeAlias td ->
            td
                |> Debug.toString
                |> (++) "type alias: "
                |> Html.text

        FA.Evaluation expr ->
            Html.div
                []
                [ Html.text "Evaluation: "
                , viewFaExpression expr
                ]

        FA.Definition { name, maybeAnnotation, parameters, body } ->
            Html.div
                []
                [ Html.span
                    [ style "font-weight" "bold" ]
                    [ let
                        (FA.PatternAny n) =
                            name
                      in
                      Html.text n
                    ]
                , maybeAnnotation
                    |> Maybe.map (\x -> " : " ++ Debug.toString x)
                    |> Maybe.withDefault ""
                    |> Html.text
                , parameters
                    |> List.map (\(FA.PatternAny n) -> n)
                    |> List.map Html.text
                    |> Html.span []
                , body
                    |> OneOrMore.toList
                    |> List.map viewFaStatement
                    |> Html.div []
                ]


viewFaExpression : FA.Expression -> Html msg
viewFaExpression expr =
    case expr of
        FA.NumberLiteral s ->
            Html.text s.number

        FA.StringLiteral s ->
            Html.text s.string

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


viewTokens : String -> Html msg
viewTokens code =
    let
        resultTokens =
            code
                |> Compiler.StringToTokens.lexer
    in
    case resultTokens of
        Err error ->
            error
                |> Types.Error.toString code
                |> Html.text

        Ok tokens ->
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
