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

    -- arithmetic
    , add
    , subtract
    , multiply
    , divide
    , mutableAdd

    -- comparison
    , lesserThan
    , greaterThan

    -- others
    , stringConcat
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
            "Text",
            "List",
            "Number"
          ]
        }, {
          "path": "SPCore/List",
          "importAs": "List",
          "globalValues": [
          ],
          "globalTypes": [
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


type alias NativeBinopArgs =
    { symbol : String
    , left : CA.Type
    , right : CA.Type
    , return : CA.Type
    , mutates : Bool
    }


nativeBinop : NativeBinopArgs -> ( String, CA.RootDef Pos )
nativeBinop ar =
    ( ar.symbol
    , CA.Value
        { pattern = CA.PatternAny ar.symbol
        , mutable = False
        , body = []
        , maybeAnnotation =
            Just
                (CA.TypeFunction
                    { from = ar.left
                    , fromIsMutable = Just False
                    , to =
                        CA.TypeFunction
                            { from = ar.right
                            , fromIsMutable = Just ar.mutates
                            , to = ar.return
                            }
                    }
                )
        }
    )


mutableAssign : ( String, CA.RootDef Pos )
mutableAssign =
    nativeBinop
        { symbol = ":="
        , left = CA.TypeVariable { name = "a" }
        , right = CA.TypeVariable { name = "a" }
        , return = Core.noneType
        , mutates = True
        }



-- arithmetic


add : ( String, CA.RootDef Pos )
add =
    nativeBinop
        { symbol = "+"
        , left = Core.numberType
        , right = Core.numberType
        , return = Core.numberType
        , mutates = False
        }


subtract : ( String, CA.RootDef Pos )
subtract =
    nativeBinop
        { symbol = "-"
        , left = Core.numberType
        , right = Core.numberType
        , return = Core.numberType
        , mutates = False
        }


multiply : ( String, CA.RootDef Pos )
multiply =
    nativeBinop
        { symbol = "*"
        , left = Core.numberType
        , right = Core.numberType
        , return = Core.numberType
        , mutates = False
        }


divide : ( String, CA.RootDef Pos )
divide =
    nativeBinop
        { symbol = "/"
        , left = Core.numberType
        , right = Core.numberType
        , return = Core.numberType
        , mutates = False
        }


mutableAdd : ( String, CA.RootDef Pos )
mutableAdd =
    nativeBinop
        { symbol = "+="
        , left = Core.numberType
        , right = Core.numberType
        , return = Core.noneType
        , mutates = True
        }



-- Comparison binops


{-| TODO I don't have a `comparable` typeclass, how do I give a type to these?
-}
lesserThan : ( String, CA.RootDef Pos )
lesserThan =
    nativeBinop
        { symbol = "<"
        , left = Core.numberType
        , right = Core.numberType
        , return = Core.boolType
        , mutates = False
        }


greaterThan : ( String, CA.RootDef Pos )
greaterThan =
    nativeBinop
        { symbol = ">"
        , left = Core.numberType
        , right = Core.numberType
        , return = Core.boolType
        , mutates = False
        }



-- Other binops


stringConcat : ( String, CA.RootDef Pos )
stringConcat =
    nativeBinop
        { symbol = ".."
        , left = Core.textType
        , right = Core.textType
        , return = Core.textType
        , mutates = False
        }


sendRight : ( String, CA.RootDef Pos )
sendRight =
    nativeBinop
        { symbol = ">>"
        , left = tyVar "a"
        , right = function (tyVar "a") (tyVar "b")
        , return = tyVar "b"
        , mutates = False
        }


sendLeft : ( String, CA.RootDef Pos )
sendLeft =
    nativeBinop
        { symbol = "<<"
        , left = function (tyVar "a") (tyVar "b")
        , right = tyVar "a"
        , return = tyVar "b"
        , mutates = False
        }



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
