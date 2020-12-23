module Compiler.TestHelpers exposing (..)

import Compiler.FormattableToCanonicalAst
import Compiler.StringToTokens
import Compiler.TokensToFormattableAst
import Dict exposing (Dict)
import Types.CanonicalAst as CA
import Types.Error
import Types.FormattableAst as FA


stringToCanonicalModule : String -> Result String (CA.Module ())
stringToCanonicalModule code =
    code
        |> unindent
        |> Compiler.StringToTokens.lexer
        |> Result.andThen Compiler.TokensToFormattableAst.parse
        |> Result.andThen Compiler.FormattableToCanonicalAst.translateModule
        |> Result.mapError (Types.Error.toString code)


stringToFormattableModule : String -> Result String FA.Module
stringToFormattableModule code =
    code
        |> unindent
        |> Compiler.StringToTokens.lexer
        |> Result.andThen Compiler.TokensToFormattableAst.parse
        |> Result.mapError (Types.Error.toString code)


unindent : String -> String
unindent multilineString =
    let
        lines =
            String.lines multilineString

        countLeadingSpaces line =
            case String.uncons line of
                Nothing ->
                    0

                Just ( char, xs ) ->
                    case char of
                        ' ' ->
                            1 + countLeadingSpaces xs

                        _ ->
                            0

        minLead =
            lines
                |> List.filter (String.any ((/=) ' '))
                |> List.map countLeadingSpaces
                |> List.minimum
                |> Maybe.withDefault 0
    in
    lines
        |> List.map (String.dropLeft minLead)
        |> String.join "\n"
