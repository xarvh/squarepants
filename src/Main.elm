module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events
import Parser
import Vier.Lexer.Chunks exposing (Chunk)
import Vier.Lexer.Error exposing (Error)
import Vier.Lexer.Indent exposing (Indented(..), IndentedChunk, StructureKind(..))
import Vier.Lexer.Token exposing (IndentedToken, OpenOrClosed(..), Token, TokenKind(..))
import Vier.Syntax as Syntax exposing (Expression)


initialCode =
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
        , Html.div
            []
            [ viewAst model.code
            , viewTokens model.code
            , Html.hr [] []
            , viewIndentedChunks model.code
            , Html.hr [] []
            , viewChunks model.code
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
                |> String.toList
                |> Array.fromList
                |> Vier.Lexer.Chunks.fromString
                |> Result.andThen Vier.Lexer.Indent.indentChunks
                |> Result.andThen (Vier.Lexer.Token.chunksToTokens code)
                |> Result.map Syntax.parse
    in
    case res of
        Ok (Just tree) ->
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
                    [ Html.text "CALL"
                    , viewAstNode fn
                    ]
                , Html.div
                    [ style "padding-left" "2em" ]
                    (List.map viewAstNode <| aHead :: aTail)
                ]

        --     Binop Expression String Expression
        --     Unop String Expression
        _ ->
            Html.code
                []
                [ expr
                    |> Debug.toString
                    |> Html.text
                ]



----
--- Chunks
--


viewTokens : String -> Html msg
viewTokens code =
    let
        resultTokens =
            code
                |> String.toList
                |> Array.fromList
                |> Vier.Lexer.Chunks.fromString
                |> Result.andThen Vier.Lexer.Indent.indentChunks
                |> Result.andThen (Vier.Lexer.Token.chunksToTokens code)
    in
    case resultTokens of
        Err err ->
            Html.pre []
                [ Html.code []
                    [ err
                        |> Vier.Lexer.Error.toString code
                        |> Html.text
                    ]
                ]

        Ok tokens ->
            tokens
                |> List.foldl addToken ( 0, [], [] )
                |> (\( depth, ts, html ) -> html)
                |> List.reverse
                |> Html.div []


addToken : IndentedToken -> ( Int, List Token, List (Html msg) ) -> ( Int, List Token, List (Html msg) )
addToken indentedToken ( depth, lineTokens, html ) =
    case indentedToken of
        Content token ->
            ( depth, token :: lineTokens, html )

        Structure NewLine ->
            ( depth
            , []
            , (lineTokens
                |> List.reverse
                |> List.map viewToken
                --                 |> (::) (viewDepth depth)
                --                 |> Html.li []
                |> Html.div [ style "margin-left" <| String.fromInt (depth * 3) ++ "em" ]
              )
                :: html
            )

        Structure BlockStart ->
            ( depth + 1, lineTokens, html )

        Structure BlockEnd ->
            ( depth - 1, lineTokens, html )


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



----
--- Chunks
--


viewChunk : String -> Chunk -> Html msg
viewChunk code chunk =
    let
        content =
            String.slice chunk.start chunk.end code

        isIndent =
            chunk.t == Vier.Lexer.Chunks.Indent
    in
    Html.code
        [ style "border" "1px solid lightgray"
        , class <| Debug.toString chunk.t
        , if isIndent then
            style "color" "lightgray"

          else
            class ""
        , Html.Attributes.id <| String.fromInt chunk.start ++ "-" ++ String.fromInt chunk.end
        ]
        [ Html.text
            (if isIndent then
                (chunk.end - chunk.start)
                    |> List.range 0
                    |> List.map String.fromInt
                    |> String.join ""
                    |> (\s -> s ++ " ")

             else
                String.slice chunk.start chunk.end code
            )
        , if chunk.t == Vier.Lexer.Chunks.ContentLine && String.endsWith "\n" content then
            Html.span
                [ style "background-color" "orange" ]
                [ Html.text "\\n" ]

          else
            Html.text ""
        ]


viewChunks : String -> Html msg
viewChunks code =
    let
        resultSemLines =
            code
                |> String.toList
                |> Array.fromList
                |> Vier.Lexer.Chunks.fromString
                |> Result.map Vier.Lexer.Chunks.toSemanticLines
    in
    case resultSemLines of
        Err err ->
            err
                |> Debug.toString
                |> Html.text

        Ok semLines ->
            semLines
                |> List.map (List.map (viewChunk code) >> Html.li [])
                |> Html.ul []



----
--- Indented Chunks
--


viewIndentedChunks : String -> Html msg
viewIndentedChunks code =
    let
        resultIndentedChunks =
            code
                |> String.toList
                |> Array.fromList
                |> Vier.Lexer.Chunks.fromString
                |> Result.andThen Vier.Lexer.Indent.indentChunks
    in
    case resultIndentedChunks of
        Err err ->
            Html.pre []
                [ Html.code []
                    [ err
                        |> Vier.Lexer.Error.toString code
                        |> Html.text
                    ]
                ]

        Ok indentedChunks ->
            indentedChunks
                |> List.foldl (addIndentedChunk code) ( 0, [], [] )
                |> (\( depth, chunks, html ) -> html)
                |> List.reverse
                |> Html.ul []


viewDepth : Int -> Html msg
viewDepth depth =
    Html.code
        [ style "color" "#eee"
        , class "depth"
        ]
        [ depth
            |> List.range 0
            |> List.map String.fromInt
            |> String.join ""
            |> Html.text
        ]


addIndentedChunk : String -> IndentedChunk -> ( Int, List Chunk, List (Html msg) ) -> ( Int, List Chunk, List (Html msg) )
addIndentedChunk code ic ( depth, chunks, html ) =
    case ic of
        Content chunk ->
            ( depth, chunk :: chunks, html )

        Structure NewLine ->
            ( depth
            , []
            , (chunks
                |> List.reverse
                |> List.map (viewChunk code)
                |> (::) (viewDepth depth)
                |> Html.li []
              )
                :: html
            )

        Structure BlockStart ->
            ( depth + 1, chunks, html )

        Structure BlockEnd ->
            ( depth - 1, chunks, html )



----
---
--


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
