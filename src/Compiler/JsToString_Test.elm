module Compiler.JsToString_Test exposing (..)

import Compiler.CanonicalToJs
import Compiler.JsToString
import Compiler.TestHelpers
import Compiler.TypeInference as TI
import Dict exposing (Dict)
import Lib
import Markdown
import Set exposing (Set)
import Test exposing (Test)
import Types.CanonicalAst as CA


tests : Test
tests =
    Test.Group "JsToString"
        [ meta
        , misc
        , mutation
        , ifs
        , try
        , natives
        ]



----
---
--


codeTest =
    Test.codeTest Debug.toString


runProgram : String -> CA.Module TI.Ext -> Result String String
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
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Result.andThen (TI.inspectModule Dict.empty)
        |> Result.mapError (Compiler.TestHelpers.errorToString code)
        |> Result.andThen (\( mod, env, sub ) -> runProgram variable mod)


meta : Test
meta =
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
            (eval "Test.a")
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
            (eval "Test.a")
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
            (eval "Test.result")
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
            (eval "Test.result")
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
            (eval "Test.result")
            (Test.okEqual """{"x":{"y":{"z":59}}}""")
        ]



----
--- ifs
--


ifs : Test
ifs =
    Test.Group "ifs"
        [ codeTest "basic sanity"
            """
            a =
              if True then
                1
              else
                2
            """
            (eval "Test.a")
            (Test.okEqual "1")
        ]



----
--- try
--


try : Test
try =
    Test.Group "try"
        [ codeTest "basic sanity"
            """
            type A = A Number, B, C Bool

            a x =
              try x as
                A 1 then 11
                A n then n
                B then 3
                C False then 5
                C _ then 6

            result =
             { x = a (A 2)
             , y = a (A 1)
             , z = a B
             , w = a (C False)
             , k = a (C True)
             }
            """
            (eval "Test.result")
            (Test.okEqual """{"k":6,"w":6,"x":2,"y":11,"z":3}""")
        ]



----
--- Natives
--


natives : Test
natives =
    Test.Group "natives"
        [ codeTest "SPCore/Debug.log"
            """
            result = log "blah" True
            """
            (eval "Test.result")
            (Test.okEqual """true""")
        , codeTest "SPCore/Debug.log, partially applied"
            """
            result = log "blah"
            """
            (eval "Test.result")
            (Test.okEqual """undefined""")
        , codeTest "SPCore/Debug.todo"
            """
            a = todo "blah"
            result = 1
            """
            (eval "Test.result")
            (Test.errContain "blah")
        ]
