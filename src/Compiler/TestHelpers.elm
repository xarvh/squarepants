module Compiler.TestHelpers exposing (..)

import Compiler.ApplyAliases
import Compiler.FormattableToCanonicalAst
import Compiler.StringToTokens
import Compiler.TokensToFormattableAst
import Dict exposing (Dict)
import Prelude exposing (meta)
import Types.CanonicalAst as CA
import Types.Error exposing (Res)
import Types.FormattableAst as FA


errorToString code =
    Types.Error.toStrings code >> String.join "\n"


resultErrorToString : String -> Res a -> Result String a
resultErrorToString code =
    Result.mapError (errorToString code)


stringToCanonicalModule : String -> Res (CA.Module ())
stringToCanonicalModule code =
    code
        |> unindent
        |> Compiler.StringToTokens.lexer
        |> Result.andThen Compiler.TokensToFormattableAst.parse
        |> Result.andThen (\fa -> Compiler.FormattableToCanonicalAst.translateModule "Test" meta fa Prelude.prelude)
        |> Result.andThen Compiler.ApplyAliases.applyAliasesToModule
        |> Result.map (\mod -> CA.extensionFold_module (\_ _ -> ( (), () )) ( mod, () ) |> Tuple.first)


stringToFormattableModule : String -> Result String FA.Module
stringToFormattableModule code =
    code
        |> unindent
        |> Compiler.StringToTokens.lexer
        |> Result.andThen Compiler.TokensToFormattableAst.parse
        |> resultErrorToString code


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
