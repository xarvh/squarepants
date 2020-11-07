module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events
import Parser
import Test
import Vier.Error
import Vier.Lexer
import Vier.Syntax as Syntax exposing (Expression)
import Vier.Syntax_Test
import Vier.Lexer_Test


initialCode =
    "a + -a"


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
        Ok tree ->
            viewAstNode tree

        _ ->
            res
                |> Debug.toString
                |> Html.text


viewAstNode : Syntax.Expression -> Html msg
viewAstNode expr =
    case expr of
        Syntax.Literal s ->
            Html.text s

        Syntax.Variable s ->
            Html.text s

        Syntax.FunctionCall fn ( aHead, aTail ) ->
            Html.div
                [ style "border" "red" ]
                [ Html.div
                    []
                    [ Html.text "[call] "
                    , viewAstNode fn
                    ]
                , Html.div
                    [ style "padding-left" "2em" ]
                    (List.map viewAstNode <| aHead :: aTail)
                ]

        Syntax.Binop left op right ->
            Html.div
                [ style "border" "red" ]
                [ Html.div
                    []
                    [ Html.text <| "[op] " ++ op ]
                , Html.div
                    [ style "padding-left" "2em" ]
                    [ viewAstNode left ]
                , Html.div
                    []
                    [ Html.text "---" ]
                , Html.div
                    [ style "padding-left" "2em" ]
                    [ viewAstNode right ]
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
