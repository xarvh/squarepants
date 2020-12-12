module Main exposing (..)

import Array exposing (Array)
import Browser
import Compiler.FormattableToCanonicalAst
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
import Test
import Types.CanonicalAst as CA
import Types.Error
import Types.FormattableAst as FA


initialCode =
    """
b : Int
b = 1
    """


itialCode =
    """
readStuff : () #> Result Error String
readStuff () =
  if readFile "stuff.json" is Ok string then string else ""


modifyThisVariable : #Int -> Void
modifyThisVariable v =
  v += 1


someValue : Int
someValue =
  a = 3
  b = 2
  return a + b


someOtherFunction : Int String #> Void
someOtherFunction n s =

  someSideEffect ()

  x = readStuff ()

  return
"""


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
        [ style "display" "flex" ]
        [ Html.textarea
            [ style "width" "50%"
            , style "height" "99vh"
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
                [ Html.h6 [] [ Html.text "AST" ]
                , viewAst model.code
                ]
            , Html.li
                []
                [ Html.h6 [] [ Html.text "Tokens" ]
                , viewTokens model.code
                ]
            , Html.li
                []
                [ Html.h6 [] [ Html.text "Tests" ]
                , [ Compiler.StringToTokens_Test.tests
                  , Compiler.TokensToFormattableAst_Test.tests
                  , Compiler.TypeInference_Test.tests
                  ]
                    |> List.concat
                    |> Test.viewList
                ]
            ]
        ]



----
--- Inference
--


viewInference : String -> Html msg
viewInference code =
    case Compiler.TestHelpers.stringToCanonicalStatements code of
        Ok scope ->
            case Compiler.TypeInference.inferScope preamble scope of
                Err err ->
                    Html.text err

                Ok env ->
                    env
                        |> Dict.toList
                        |> List.map (\( k, v ) -> Html.div [] [ k ++ ": " ++ Debug.toString v |> Html.text ])
                        |> Html.div []

        Err error ->
            error
                |> Debug.toString
                |> (++) "ERROR "
                |> Html.text



----
--- AST
--


viewAst : String -> Html msg
viewAst code =
    let
        res =
            code
                |> Compiler.StringToTokens.lexer
                |> Result.andThen Compiler.TokensToFormattableAst.parse
    in
    case res of
        Ok statements ->
            statements
                |> List.map viewRootStatement
                |> Html.div []

        _ ->
            res
                |> Debug.toString
                |> Html.text


viewRootStatement : FA.RootStatement -> Html msg
viewRootStatement rs =
    case rs of
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

        FA.Statement s ->
            viewStatement s


viewStatement : FA.Statement -> Html msg
viewStatement s =
    case s of
        FA.Evaluate expr ->
            Html.div
                []
                [ Html.text "evaluate: "
                , viewExpression expr
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
                    |> List.map viewStatement
                    |> Html.div []
                ]

        FA.Mutation x ->
            Debug.toString x
                |> Html.text


viewExpression : FA.Expression -> Html msg
viewExpression expr =
    case expr of
        FA.NumberLiteral s ->
            Html.text s.number

        FA.StringLiteral s ->
            Html.text s.string

        FA.Variable s ->
            Html.text s.variable

        FA.FunctionCall { reference, arguments } ->
            Html.div
                [ style "border" "red" ]
                [ Html.div
                    []
                    [ Html.text "[call] "
                    , viewExpression reference
                    ]
                , Html.div
                    [ style "padding-left" "2em" ]
                    (List.map viewExpression <| Tuple.first arguments :: Tuple.second arguments)
                ]

        FA.Binop { left, op, right } ->
            Html.div
                [ style "border" "red" ]
                [ Html.div
                    []
                    [ Html.text <| "[op] " ++ op ]
                , Html.div
                    [ style "padding-left" "2em" ]
                    [ viewExpression left ]
                , Html.div
                    []
                    [ Html.text "---" ]
                , Html.div
                    [ style "padding-left" "2em" ]
                    [ viewExpression right ]
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



{-
   viewToken : Token -> Html msg
   viewToken token =
       let
           key s =
               ( s, s, "blue" )

           ( content, className, color ) =
               case token.kind of
                   StringLiteral s ->
                       ( s, "string", "pink" )

                   NumberLiteral s ->
                       ( s, "number", "pink" )

                   Symbol s ->
                       ( s
                       , "symbol"
                       , if startsWithUpper s then
                           "green"

                         else
                           "#222"
                       )

                   If ->
                       key "if"

                   Is ->
                       key "is"

                   Then ->
                       key "then"

                   Else ->
                       key "else"

                   Return ->
                       key "return"

                   Binop s ->
                       ( s, "binop", "orange" )

                   Unop s ->
                       ( s, "unop", "red" )

                   RoundParen Open ->
                       ( "(", "paren", "purple" )

                   RoundParen Closed ->
                       ( ")", "paren", "purple" )

                   _ ->
                       ( Debug.toString token, "", "" )
       in
       Html.span
           []
           [ Html.code
               [ style "border" "1px solid #eee"
               , style "color" color
               , style "margin-left" "0.5em"
               , class className
               , Html.Attributes.id <| String.fromInt token.start ++ "-" ++ String.fromInt token.end
               ]
               [ Html.text content ]
           ]


   startsWithUpper : String -> Bool
   startsWithUpper s =
       case String.uncons s of
           Just ( char, rest ) ->
               char >= 'A' && char <= 'Z'

           Nothing ->
               False

-}
----
---
--


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
