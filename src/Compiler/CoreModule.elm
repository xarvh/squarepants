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
        ]
            |> List.foldl (\u -> Dict.insert u.name u) Dict.empty
    , values =
        Dict.empty
    }



----
--- None
--


none : CA.UnionDef
none =
    { name = "None"
    , args = []
    , constructors =
        [ { name = "None"
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


bool : CA.UnionDef
bool =
    { name = "Bool"
    , args = []
    , constructors =
        [ { name = "True", args = [] }
        , { name = "False", args = [] }
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
