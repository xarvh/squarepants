module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events
import Parse.Chunks exposing (Chunk)
import Parse.Error exposing (Error)
import Parse.Indent exposing (Indented(..), IndentedChunk)
import Parse.Token exposing (IndentedToken, OpenOrClosed(..), Token(..))


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
            [ viewTokens model.code
            , Html.hr [] []
            , viewIndentedChunks model.code
            , Html.hr [] []
            , viewChunks model.code
            ]
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
                |> Parse.Chunks.fromString
                |> Result.andThen Parse.Indent.indentChunks
                |> Result.andThen (Parse.Token.chunksToTokens code)
    in
    case resultTokens of
        Err err ->
            Html.pre []
                [ Html.code []
                    [ err
                        |> Parse.Error.toString code
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
        NormalChunk token ->
            ( depth, token :: lineTokens, html )

        NewLine ->
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

        BlockStart ->
            ( depth + 1, lineTokens, html )

        BlockEnd ->
            ( depth - 1, lineTokens, html )


viewToken : Token -> Html msg
viewToken token =
    let
        key s =
            ( s, s, "blue" )

        ( content, className, color ) =
            case token of
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
            chunk.t == Parse.Chunks.Indent
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
        , if chunk.t == Parse.Chunks.ContentLine && String.endsWith "\n" content then
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
                |> Parse.Chunks.fromString
                |> Result.map Parse.Chunks.toSemanticLines
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
                |> Parse.Chunks.fromString
                |> Result.andThen Parse.Indent.indentChunks
    in
    case resultIndentedChunks of
        Err err ->
            Html.pre []
                [ Html.code []
                    [ err
                        |> Parse.Error.toString code
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
        NormalChunk chunk ->
            ( depth, chunk :: chunks, html )

        NewLine ->
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

        BlockStart ->
            ( depth + 1, chunks, html )

        BlockEnd ->
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
