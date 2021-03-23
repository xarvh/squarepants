module Compiler.Pipeline exposing (..)

import Compiler.FormattableToCanonicalAst
import Compiler.StringToTokens
import Compiler.TokensToFormattableAst
import Types.CanonicalAst as CA
import Types.Error exposing (Res)
import Types.FormattableAst as FA
import Types.Meta exposing (Meta)
import Types.Token exposing (Token)




andThenMapError : (e -> f) -> (a -> Result e b) -> Result f a -> Result f b
andThenMapError transformError f =
    Result.andThen (f >> Result.mapError transformError)


stringToTokens : String -> String -> Res (List Token)
stringToTokens moduleName code =
    code
        |> Compiler.StringToTokens.lexer


stringToFormattableAst : String -> String -> Res FA.Module
stringToFormattableAst moduleName code =
    code
        |> stringToTokens moduleName
        |> Result.andThen (Compiler.TokensToFormattableAst.parse moduleName code)


stringToCanonicalAst : Meta -> String -> String -> CA.Module CA.Pos -> Res (CA.Module CA.Pos)
stringToCanonicalAst meta moduleName code accum =
    code
        |> stringToFormattableAst moduleName
        |> Result.andThen (\fa -> Compiler.FormattableToCanonicalAst.translateModule moduleName meta fa accum)


