module Compiler.JsToString_Test exposing (..)

import Compiler.CanonicalToJs
import Compiler.JsToString
import Compiler.TestHelpers
import Dict exposing (Dict)
import Markdown
import Test exposing (Test)
import Types.CanonicalAst as CA


tests : Test
tests =
    Test.Group "JsToString"
        [ self
        ]



----
---
--


codeTest =
    Test.codeTest Debug.toString


runProgram : String -> CA.AllDefs -> Result String String
runProgram variable mod =
    let
        endStatements =
            [ Compiler.CanonicalToJs.translatePath variable ++ ";" ]

        evalToResult stats s =
            if String.startsWith "eval()" s then
                (s :: "" :: stats)
                    |> String.join "\n\n"
                    |> Err

            else
                Ok s
    in
    mod
        |> Compiler.CanonicalToJs.translateAll
        |> List.map (Compiler.JsToString.emitStatement 0)
        |> (\stats ->
                (Compiler.CanonicalToJs.nativeDefinitions :: stats ++ endStatements)
                    |> String.join "\n\n"
                    |> Markdown.eval
                    |> evalToResult stats
           )


eval : String -> String -> Result String String
eval variable code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModuleWithPos
        --|> Result.andThen (TI.inspectModule Dict.empty)
        |> Compiler.TestHelpers.resErrorToString code
        |> Result.andThen (\mod -> runProgram variable mod)


self : Test
self =
    Test.Group "Markdown.eval workaround itself works"
        [ codeTest "base"
            """
            x = 1 + 1
            """
            (eval "Test.x")
            (Test.okEqual "2")
        , codeTest "None is null"
            """
            x = None
            """
            (eval "Test.x")
            (Test.okEqual "null")
        , codeTest "undefined reference"
            """
            x = None
            """
            (eval "Test.y")
            (Test.errContain "not defined")
        ]
