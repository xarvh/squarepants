module Compiler.CoreModule exposing (..)

import Dict exposing (Dict)
import Types.CanonicalAst as CA


{-| This module contains all the types that are necessary for the syntax.
-}
coreModule : CA.Module ()
coreModule =
    { aliases =
        Dict.empty
    , unions =
        [ none
        , bool
        , list
        , text
        , number
        , char
        ]
            |> List.foldl (\u -> Dict.insert u.name u) Dict.empty
    , values =
        []
    }



----
--- Text
--


text : CA.UnionDef
text =
    { name = "Text"
    , args = []
    , constructors = []
    }


textType : CA.Type
textType =
    CA.TypeConstant
        { path = text.name
        , args = []
        }



----
--- Number
--


number : CA.UnionDef
number =
    { name = "Number"
    , args = []
    , constructors = []
    }


numberType : CA.Type
numberType =
    CA.TypeConstant
        { path = number.name
        , args = []
        }



----
--- Char
--


char : CA.UnionDef
char =
    { name = "Char"
    , args = []
    , constructors = []
    }


charType : CA.Type
charType =
    CA.TypeConstant
        { path = char.name
        , args = []
        }



----
--- None
--


noneValue : CA.Name
noneValue =
    "None"


none : CA.UnionDef
none =
    { name = "None"
    , args = []
    , constructors =
        [ { name = noneValue
          , args = []
          }
        ]
    }


noneType : CA.Type
noneType =
    CA.TypeConstant
        { path = none.name
        , args = []
        }



----
--- Bool
--


trueValue : CA.Name
trueValue =
    "True"


falseValue : CA.Name
falseValue =
    "False"


bool : CA.UnionDef
bool =
    { name = "Bool"
    , args = []
    , constructors =
        [ { name = trueValue, args = [] }
        , { name = falseValue, args = [] }
        ]
    }


boolType : CA.Type
boolType =
    CA.TypeConstant
        { path = bool.name
        , args = []
        }



----
--- List
--


list : CA.UnionDef
list =
    { name = "List"
    , args = [ "item" ]
    , constructors =
        [ listNil
        , listCons
        ]
    }


listNil : CA.UnionConstructor
listNil =
    { name = "Nil"
    , args = []
    }


listCons : CA.UnionConstructor
listCons =
    { name = "Cons"
    , args =
        [ CA.TypeVariable
            { name = "item"
            }
        , CA.TypeConstant
            { path = "List"
            , args = [ CA.TypeVariable { name = "item" } ]
            }
        ]
    }


nil : CA.Expression ()
nil =
    CA.Variable ()
        { start = 0
        , end = 0
        , path = listNil.name
        , attrPath = []
        }


cons : CA.Expression ()
cons =
    CA.Variable ()
        { start = 0
        , end = 0
        , path = listCons.name
        , attrPath = []
        }
