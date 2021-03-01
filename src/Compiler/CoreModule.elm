module Compiler.CoreModule exposing (..)

import Dict exposing (Dict)
import Types.CanonicalAst as CA exposing (Pos)


moduleName =
    "SPCore"


root name =
    moduleName ++ "." ++ name


todoPos : Pos
todoPos =
    ( -2, -2 )


{-| This module contains all the types that are necessary for the syntax.

TODO rename to CoreTypes?

-}
coreModule : CA.Module Pos
coreModule =
    let
        u uDef =
            ( uDef.name, CA.Union uDef )
    in
    [ u none
    , u bool
    , u list
    , u text
    , u number
    , u char
    ]
        |> Dict.fromList



----
--- Text
--


text : CA.UnionDef
text =
    { name = root "Text"
    , args = []
    , constructors = Dict.empty
    }


textType : CA.Type
textType =
    CA.TypeConstant
        { ref = text.name
        , args = []
        }



----
--- Number
--


number : CA.UnionDef
number =
    { name = root "Number"
    , args = []
    , constructors = Dict.empty
    }


numberType : CA.Type
numberType =
    CA.TypeConstant
        { ref = number.name
        , args = []
        }



----
--- Char
--


char : CA.UnionDef
char =
    { name = root "Char"
    , args = []
    , constructors = Dict.empty
    }


charType : CA.Type
charType =
    CA.TypeConstant
        { ref = char.name
        , args = []
        }



----
--- None
--


noneValue : String
noneValue =
    root "None"


none : CA.UnionDef
none =
    { name = root "None"
    , args = []
    , constructors =
        [ { name = noneValue
          , args = []
          }
        ]
            |> List.map (\c -> ( c.name, c.args ))
            |> Dict.fromList
    }


noneType : CA.Type
noneType =
    CA.TypeConstant
        { ref = none.name
        , args = []
        }



----
--- Bool
--


trueValue : String
trueValue =
    root "True"


falseValue : String
falseValue =
    root "False"


bool : CA.UnionDef
bool =
    { name = root "Bool"
    , args = []
    , constructors =
        [ { name = trueValue, args = [] }
        , { name = falseValue, args = [] }
        ]
            |> List.map (\c -> ( c.name, c.args ))
            |> Dict.fromList
    }


boolType : CA.Type
boolType =
    CA.TypeConstant
        { ref = bool.name
        , args = []
        }



----
--- List
--


list : CA.UnionDef
list =
    { name = root "List"
    , args = [ "item" ]
    , constructors =
        [ listNil
        , listCons
        ]
            |> List.map (\c -> ( c.name, c.args ))
            |> Dict.fromList
    }



-- listNil : CA.UnionConstructor


listNil =
    { name = root "Nil"
    , args = []
    }



-- listCons : CA.UnionConstructor


listCons =
    { name = root "Cons"
    , args =
        [ CA.TypeVariable
            { name = "item"
            }
        , CA.TypeConstant
            { ref = root "List"
            , args = [ CA.TypeVariable { name = "item" } ]
            }
        ]
    }


nil : CA.Expression Pos
nil =
    CA.Variable todoPos
        { isRoot = True
        , name = listNil.name
        , attrPath = []
        }


cons : CA.Expression Pos
cons =
    CA.Variable todoPos
        { isRoot = True
        , name = listCons.name
        , attrPath = []
        }
