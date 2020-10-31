module Vier.Syntax exposing (..)

import Parser exposing (do, oneOf, succeed)
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


term : Parser Expression
term =
    Parser.fromFn <| \indentedToken ->
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



{- Precedence rules:

   f a b ------------> function application

   not, risk --------> unary

   ^ ----------------> exp

   * / --------------> multiplicative

   + - ++ -----------> addittive

   >= <= == =/= -----> comparison

   and, or, xor -----> logical

   |> <| >> << ------> pipes

   := += -= /= *= ---> assignments

-}


boundExprOrTerm : Parser Expression
boundExprOrTerm =
    oneOf
        [ term
        , -- ( expr )
          do openParen <| \_ ->
          do expr <| \e ->
          do closedParen <| \_ ->
          succeed e
        ]


functionApplicationOrTerm : Parser Expression
functionApplicationOrTerm =
    do boundExprOrTerm <| \e ->
    do (oneOrMore boundExprOrTerm) <| \es ->
    FunctionCall e es


expr : Parser Expression
expr =
    -- https://github.com/glebec/left-recursion
    do exprStart <| \startExpr ->
    do exprEnd <| \maybeEnd ->
    case maybeEnd of
        Nothing ->
            succeed startExpr

        Just combinator ->
            succeed (combinator startExpr)


{-| This is the part of expr that does NOT have recursion
-}
exprStart : Parser Expression
exprStart =
    Parser.oneOf
        [ Parser.fromFn maybeLiteral
        , unop
        ]


{-| This is the part of expr that DOES have recursion
-}
exprEnd : Parser (Maybe (Expression -> Expression))
exprEnd =
    Parser.oneOf
        [ -- Binop
          do binop <| \op ->
          do expr <| \rightExpr ->
          succeed <| Just <| \startExpr -> Binop startExpr op rightExpr
        , -- Function call
          do (Parser.oneOrMore expr) <| \args ->
          succeed <| Just <| \startExpr -> FunctionCall startExpr args
        , succeed Nothing
        ]


binop : Parser String
binop =
    [ "+"
    , "*"
    ]
        |> List.map (maybeBinop >> Parser.fromFn)
        |> Parser.oneOf


maybeBinop : String -> IndentedToken -> Maybe String
maybeBinop op it =
    case it of
        Indent.Content token ->
            case token.kind of
                Token.Binop s ->
                    if op == s then
                        Just s

                    else
                        Nothing

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
    succeed <| Unop op right
