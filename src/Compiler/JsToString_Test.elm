module Compiler.JsToString_Test exposing (..)

import Compiler.CanonicalToJs
import Compiler.JsToString
import Compiler.TestHelpers
import Compiler.TypeInference as TI
import Compiler.TypeInference_Test exposing (preamble)
import Dict exposing (Dict)
import Lib
import Markdown
import Set exposing (Set)
import Test exposing (Test)


tests : Test
tests =
    Test.Group "JsToString"
        [ meta
        , misc
        , mutation
        ]



----
---
--


codeTest =
    Test.codeTest Debug.toString


eval : String -> String -> Result String String
eval variable code =
    let
        endStatements =
            [ Compiler.CanonicalToJs.translatePath variable ++ ";" ]

        inferredToEvaluatedString ( mod, env, subs ) =
            [ mod ]
                |> Compiler.CanonicalToJs.translateAll
                |> List.map (Compiler.JsToString.emitStatement 0)
                |> (\stats ->
                        (Compiler.CanonicalToJs.cloneDefinition :: stats ++ endStatements)
                            |> String.join "\n\n"
                            |> Markdown.eval
                            |> evalToResult stats
                   )

        evalToResult stats s =
            if String.startsWith "eval()" s then
                (s :: "" :: stats)
                    |> String.join "\n\n"
                    |> Err

            else
                Ok s
    in
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Result.andThen (TI.inspectModule preamble)
        |> Result.mapError (Compiler.TestHelpers.errorToString code)
        |> Result.andThen inferredToEvaluatedString


meta : Test
meta =
    Test.Group "Markdown.eval workaround itself works"
        [ codeTest "base"
            """
            x = 1 + 1
            """
            (eval "x")
            (Test.okEqual "2")
        , codeTest "None is null"
            """
            x = None
            """
            (eval "x")
            (Test.okEqual "null")
        , codeTest "undefined reference"
            """
            x = None
            """
            (eval "y")
            (Test.errContain "not defined")
        ]



----
--- Misc stuff
--


misc : Test
misc =
    Test.Group "misc"
        [ codeTest "definitions and mutations return None"
            """
            x =
              m @= 0

            y =
              m @= 0
              @m += 1

            a =
              { x, y }
            """
            (eval "a")
            (Test.okEqual """{"x":null,"y":null}""")
        ]



----
--- Mutation
--


mutation : Test
mutation =
    Test.Group "mutation"
        [ codeTest "basic sanity"
            """
            a =
              m @= 0
              @m += 1
              x = m
              @m := 10
              y = m
              @m += 1
              z = m
              { x, y, z, m }
            """
            (eval "a")
            (Test.okEqual """{"m":11,"x":1,"y":10,"z":11}""")

        --
        , codeTest "nested record"
            """
            record = { x = { y = { z = 4 } } }

            result =
               m @= record
               @m.x.y :=  { z = 1 }
               @m.x.y.z += 1
               m
            """
            (eval "result")
            (Test.okEqual """{"x":{"y":{"z":2}}}""")

        --
        , codeTest "pass mutable to function"
            """
            fun m =
              @m += 55

            result =
               m @= 2
               fun @m
               m
            """
            (eval "result")
            (Test.okEqual """57""")

        --
        , codeTest "pass nested mutable value to function"
            """
            fun m =
              @m += 55

            record = { x = { y = { z = 4 } } }

            result =
               m @= record
               fun @m.x.y.z
               m
            """
            (eval "result")
            (Test.okEqual """{"x":{"y":{"z":59}}}""")
        ]
