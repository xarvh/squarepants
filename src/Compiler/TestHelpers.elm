module Compiler.TestHelpers exposing (..)

import Compiler.FormattableToCanonicalAst
import Compiler.StringToTokens
import Compiler.TokensToFormattableAst
import Dict exposing (Dict)
import Types.CanonicalAst as CA
import Types.Error
import Types.FormattableAst as FA


insertStatement : FA.RootStatement -> Dict String (CA.Expression ()) -> Dict String (CA.Expression ())
insertStatement rootStatement scope =
    case rootStatement of
        FA.Statement statement ->
            case Compiler.FormattableToCanonicalAst.statement statement of
                Ok (CA.Definition { name, body }) ->
                    Dict.insert name body scope

                _ ->
                    scope

        _ ->
            scope


stringToCanonicalStatements : String -> Result String (Dict String (CA.Expression ()))
stringToCanonicalStatements code =
    code
        |> Compiler.StringToTokens.lexer
        |> Result.andThen Compiler.TokensToFormattableAst.parse
        |> Result.map (List.foldl insertStatement Dict.empty)
        |> Result.mapError (Types.Error.toString code)
