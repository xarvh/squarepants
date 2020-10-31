module Vier.Syntax exposing (..)

import Parser exposing (do, return)
import Vier.Lexer.Indent as Indent
import Vier.Lexer.Token as Token exposing (IndentedToken, Token)


type alias Parser a =
    Parser.Parser IndentedToken (List IndentedToken) a


type Expression
    = Literal String
    | Variable String
    | FunctionCall Expression ( Expression, List Expression )
    | Binop Expression String Expression
    | Unop String Expression
    | If { condition : Expression, true : Expression, false : Expression }
    | Error


parse : List IndentedToken -> Maybe Expression
parse =
    let
        uncons : List a -> Maybe ( a, List a )
        uncons ls =
            case ls of
                head :: tail ->
                    Just ( head, tail )

                [] ->
                    Nothing
    in
    -- TODO remove the drop 1, which is there to remove a spurious newline
    List.drop 1 >> expr uncons >> Maybe.map Tuple.first


expr : Parser Expression
expr =
    -- https://github.com/glebec/left-recursion
    do exprStart <| \startExpr ->
    do exprEnd <| \maybeEnd ->
    case maybeEnd of
        Nothing ->
            return startExpr

        Just combinator ->
            return (combinator startExpr)


exprStart : Parser Expression
exprStart =
    Parser.oneOf
        [ Parser.fromFn maybeLiteral
        , unop
        ]


exprEnd : Parser (Maybe (Expression -> Expression))
exprEnd =
    Parser.oneOf
        [ -- Binop
          do (Parser.fromFn maybeBinop) <| \binop ->
          do expr <| \rightExpr ->
          (\startExpr -> Binop startExpr binop rightExpr)
              |> Just
              |> return
        , -- Function call
          do (Parser.oneOrMore expr) <| \args ->
          (\startExpr -> FunctionCall startExpr args)
              |> Just
              |> return
        , return Nothing
        ]


maybeLiteral : IndentedToken -> Maybe Expression
maybeLiteral it =
    case it of
        Indent.Structure structure ->
            Nothing

        Indent.Content token ->
            case token.kind of
                Token.NumberLiteral s ->
                    Just <| Literal s

                Token.StringLiteral s ->
                    Just <| Literal s

                Token.Symbol s ->
                    Just <| Variable s

                _ ->
                    Nothing


maybeBinop : IndentedToken -> Maybe String
maybeBinop it =
    case it of
        Indent.Content token ->
            case token.kind of
                Token.Binop s ->
                    Just s

                _ ->
                    Nothing

        _ ->
            Nothing


maybeUnop : IndentedToken -> Maybe String
maybeUnop it =
    case it of
        Indent.Content token ->
            case token.kind of
                Token.Unop s ->
                    Just s

                _ ->
                    Nothing

        _ ->
            Nothing


unop : Parser Expression
unop =
    do (Parser.fromFn maybeUnop) <| \op ->
    do expr <| \right ->
    return <| Unop op right
