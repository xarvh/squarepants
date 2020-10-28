module Vier.Syntax exposing (..)

import Parser exposing (do, return)
import Vier.Lexer.Indent as Indent
import Vier.Lexer.Token as Token exposing (IndentedToken, Token)


type alias Parser a =
    Parser.Parser IndentedToken a


type Expression
    = Literal String
    | Variable String
    | FunctionCall Expression ( Expression, List Expression )
    | Binop Expression String Expression
    | Unop String Expression
    | If { condition : Expression, true : Expression, false : Expression }
    | Error


expr : Parser Expression
expr =
    Parser.breakCircularReference <| \_ ->
    Parser.oneOf
        [ Parser.fromFn maybeAtom
        , functionCall
        , unop
        , binop
        ]


maybeAtom : IndentedToken -> Maybe Expression
maybeAtom it =
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


functionCall : Parser Expression
functionCall =
    Parser.breakCircularReference <| \_ ->
    do expr <| \functionRef ->
    do expr <| \firstArg ->
    do (Parser.zeroOrMore expr) <| \otherArgs ->
    return <| FunctionCall functionRef ( firstArg, otherArgs )


binop : Parser Expression
binop =
    Parser.breakCircularReference <| \_ ->
    do expr <| \left ->
    do (Parser.fromFn maybeBinop) <| \op ->
    do expr <| \right ->
    return <| Binop left op right


unop : Parser Expression
unop =
    Parser.breakCircularReference <| \_ ->
    do (Parser.fromFn maybeUnop) <| \op ->
    do expr <| \right ->
    return <| Unop op right
