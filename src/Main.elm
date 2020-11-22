module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events
import OneOrMore exposing (OneOrMore)
import Parser
import Test
import Vier.Error
import Vier.Lexer
import Vier.Lexer_Test
import Vier.Syntax as Syntax exposing (Expression)
import Vier.Syntax_Test
import Vier.TypeInference


initialCode =
    """
a x =
 1
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
                , Test.viewList (Vier.Lexer_Test.tests ++ Vier.Syntax_Test.tests)
                ]
            ]
        ]



----
--- Inference
--


viewInference : String -> Html msg
viewInference code =
    let
        res =
            code
                |> Vier.Lexer.lexer
                |> Result.andThen Syntax.parse
    in
    case res of
        Ok (statementsHead :: statementsTail) ->
            Html.div
                []
                [ ( statementsHead, [] )
                    |> Vier.TypeInference.inferStatements Vier.TypeInference.initContext
                    |> Debug.toString
                    |> Html.text
                ]

        _ ->
            res
                |> Debug.toString
                |> Html.text



----
--- AST
--


viewAst : String -> Html msg
viewAst code =
    let
        res =
            code
                |> Vier.Lexer.lexer
                |> Result.andThen Syntax.parse
    in
    case res of
        Ok statements ->
            statements
                |> List.map viewStatement
                |> Html.div []

        _ ->
            res
                |> Debug.toString
                |> Html.text


viewStatement : Syntax.Statement -> Html msg
viewStatement statement =
    case statement of
        Syntax.Pass ->
            Html.text "pass"

        Syntax.Evaluate expr ->
            Html.div
                []
                [ Html.text "evaluate: "
                , viewExpression expr
                ]

        Syntax.Definition { name, parameters, body } ->
            Html.div
                []
                [ Html.span
                    [ style "font-weight" "bold" ]
                    [ Html.text name ]
                , parameters
                    |> List.map Html.text
                    |> Html.span []
                , body
                    |> OneOrMore.toList
                    |> List.map viewStatement
                    |> Html.div []
                ]

        Syntax.Return expr ->
            Html.div
                []
                [ Html.span [] [ Html.text "return " ]
                , viewExpression expr
                ]

        Syntax.If_Imperative { condition, true, false } ->
            Debug.todo "If_Imperative"

        Syntax.Match_Imperative { value, patterns, maybeElse } ->
            Debug.todo "Match_Imperative"


viewExpression : Syntax.Expression -> Html msg
viewExpression expr =
    case expr of
        Syntax.NumberLiteral s ->
            Html.text s

        Syntax.StringLiteral s ->
            Html.text s

        Syntax.Variable s ->
            Html.text s

        Syntax.FunctionCall { reference, arguments } ->
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

        Syntax.Binop left op right ->
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
                |> Vier.Lexer.lexer
    in
    case resultTokens of
        Err error ->
            error
                |> Vier.Error.toString code
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
