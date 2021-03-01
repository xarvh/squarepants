module Prelude exposing (..)

import Compiler.CoreModule as Core
import Dict exposing (Dict)
import MetaFile exposing (MetaFile)
import Types.CanonicalAst as CA exposing (Pos, Type)
import Types.Meta exposing (Meta)



----
--- Prelude
--


prelude : CA.Module Pos
prelude =
    [ mutableAssign
    , binaryAdd
    , mutableAdd

    -- pipes
    , sendRight
    , sendLeft

    -- debug
    , debugTodo
    , debugLog
    ]
        |> List.foldl (\( n, v ) -> Dict.insert n v) Core.coreModule



----
--- Meta
--


metaString =
    """
    {
      "sourceDirs": [{
        "path": "",
        "moduleExceptions": [{
          "path": "SPCore",
          "importAs": "SPCore",
          "globalValues": [
            "None",
            "True",
            "False"
          ],
          "globalTypes": [
            "None",
            "Bool",
            "List",
            "Number"
          ]
        }, {
          "path": "SPCore/Maybe",
          "importAs": "Maybe",
          "globalValues": [
            "Just",
            "Nothing"
          ],
          "globalTypes": [
            "Maybe"
          ]
        }, {
          "path": "SPCore/Debug",
          "importAs": "Debug",
          "globalValues": [
            "log",
            "todo"
          ],
          "globalTypes": [
          ]
        }]
      }],
      "libraries": []
    }
    """


metaFile : MetaFile
metaFile =
    case MetaFile.stringToMetaFile metaString of
        Ok f ->
            f

        Err error ->
            Debug.todo <| "Prelude MetaFile error: " ++ error


meta : Meta
meta =
    MetaFile.toMeta metaFile



----
---
--


tyVar n =
    CA.TypeVariable { name = n }


constant n =
    CA.TypeConstant { ref = n, args = [] }


function from to =
    CA.TypeFunction { from = from, fromIsMutable = Nothing, to = to }



----
--- Binops
--


mutableAssign : ( String, CA.RootDef Pos )
mutableAssign =
    ( ":="
    , CA.Value
        { pattern = CA.PatternAny ":="
        , mutable = False
        , body = []
        , maybeAnnotation =
            Just
                (CA.TypeFunction
                    { from = CA.TypeVariable { name = "a" }
                    , fromIsMutable = Just False
                    , to =
                        CA.TypeFunction
                            { from = CA.TypeVariable { name = "a" }
                            , fromIsMutable = Just True
                            , to = Core.noneType
                            }
                    }
                )
        }
    )


binaryAdd : ( String, CA.RootDef Pos )
binaryAdd =
    ( "+"
    , CA.Value
        { pattern = CA.PatternAny "+"
        , mutable = False
        , body = []
        , maybeAnnotation =
            Just
                (function
                    Core.numberType
                    (function
                        Core.numberType
                        Core.numberType
                    )
                )
        }
    )


mutableAdd : ( String, CA.RootDef Pos )
mutableAdd =
    ( "+="
    , CA.Value
        { pattern = CA.PatternAny "+="
        , mutable = False
        , body = []
        , maybeAnnotation =
            Just
                (CA.TypeFunction
                    { from = Core.numberType
                    , fromIsMutable = Just False
                    , to =
                        CA.TypeFunction
                            { from = Core.numberType
                            , fromIsMutable = Just True
                            , to = Core.noneType
                            }
                    }
                )
        }
    )



----
--- Pipes
--


sendRight : ( String, CA.RootDef Pos )
sendRight =
    ( ">>"
    , CA.Value
        { pattern = CA.PatternAny ">>"
        , mutable = False
        , body = []
        , maybeAnnotation =
            Just
                (function
                    (function (tyVar "a") (tyVar "b"))
                    (function
                        (tyVar "a")
                        (tyVar "b")
                    )
                )
        }
    )


sendLeft : ( String, CA.RootDef Pos )
sendLeft =
    ( "<<"
    , CA.Value
        { pattern = CA.PatternAny "<<"
        , mutable = False
        , body = []
        , maybeAnnotation =
            Just
                (function
                    (tyVar "a")
                    (function
                        (function (tyVar "a") (tyVar "b"))
                        (tyVar "b")
                    )
                )
        }
    )



----
--- SPCore/Debug
--


debugTodo : ( String, CA.RootDef Pos )
debugTodo =
    ( "SPCore/Debug.todo"
    , CA.Value
        { pattern = CA.PatternAny "SPCore/Debug.todo"
        , mutable = False
        , body = [{- TODO -}]
        , maybeAnnotation =
            Just
                (CA.TypeFunction
                    { from = Core.textType
                    , fromIsMutable = Just False
                    , to = CA.TypeVariable { name = "a" }
                    }
                )
        }
    )


debugLog : ( String, CA.RootDef Pos )
debugLog =
    ( "SPCore/Debug.log"
    , CA.Value
        { pattern = CA.PatternAny "SPCore/Debug.log"
        , mutable = False
        , body = [{- TODO -}]
        , maybeAnnotation =
            Just
                (CA.TypeFunction
                    { from = Core.textType
                    , fromIsMutable = Just False
                    , to =
                        CA.TypeFunction
                            { from = CA.TypeVariable { name = "a" }
                            , fromIsMutable = Just False
                            , to = CA.TypeVariable { name = "a" }
                            }
                    }
                )
        }
    )
