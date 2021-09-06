module Compiler.TestHelpers exposing (..)

import Compiler.ApplyAliases
import Compiler.Pipeline
import Dict exposing (Dict)
import Prelude exposing (meta)
import StateMonad as M exposing (M, do, return)
import Types.CanonicalAst as CA
import Types.Error as Error exposing (Res)
import Types.FormattableAst as FA


moduleName =
    "Test"


p : CA.Pos
p =
    CA.T


type alias CA_Fold_Function target acc =
    (CA.PosMap -> CA.Pos -> M acc CA.Pos) -> target -> M acc target


removePos : CA_Fold_Function target () -> target -> target
removePos posMap_something target =
    ()
        |> posMap_something (\_ _ -> return CA.T) target
        |> Tuple.first


errorToString : Error.ErrorEnv -> Error.Error -> String
errorToString eenv e =
    Error.flatten e []
        |> List.map (Error.toString eenv)
        |> String.join "\n\n"


resErrorToString : String -> Res a -> Result String a
resErrorToString code =
    let
        eenv =
            { metaFile = { sourceDirs = [], libraries = [] }
            , moduleByName = Dict.singleton "Test" { fsPath = "<TestPath>", content = unindent code }
            }
    in
    Result.mapError (errorToString eenv)


stringToCanonicalModuleWithPos : String -> Res CA.AllDefs
stringToCanonicalModuleWithPos code =
    code
        |> unindent
        |> (\c -> Compiler.Pipeline.stringToCanonicalAst meta moduleName c)
        |> Result.map (Dict.union Prelude.prelude)
        |> Result.andThen Compiler.ApplyAliases.applyAliasesToModule


stringToCanonicalModule : String -> Res CA.AllDefs
stringToCanonicalModule code =
    code
        |> stringToCanonicalModuleWithPos
        |> Result.map (removePos CA.posMap_module)


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
