module Compiler.TestHelpers exposing (..)

import Compiler.ApplyAliases
import Compiler.Pipeline
import Dict exposing (Dict)
import Prelude exposing (meta)
import Types.CanonicalAst as CA
import Types.Error as Error exposing (Res)
import Types.FormattableAst as FA


moduleName =
    "Test"


resErrorToString : Res a -> Result String a
resErrorToString =
    Result.mapError (\e -> Error.flatten e [] |> List.map Error.toString |> String.join "\n\n")


stringToCanonicalModule : String -> Res (CA.Module ())
stringToCanonicalModule code =
    code
        |> unindent
        |> (\c -> Compiler.Pipeline.stringToCanonicalAst meta moduleName c Prelude.prelude)
        |> Result.andThen Compiler.ApplyAliases.applyAliasesToModule
        |> Result.map (\mod -> CA.extensionFold_module (\_ _ -> ( (), () )) ( mod, () ) |> Tuple.first)


stringToFormattableModule : String -> Res FA.Module
stringToFormattableModule code =
    code
        |> unindent
        |> Compiler.Pipeline.stringToFormattableAst moduleName


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
