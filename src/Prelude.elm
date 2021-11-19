module Prelude exposing (..)

import Compiler.CoreModule as Core
import Dict exposing (Dict)
import Types.CanonicalAst as CA exposing (Type)
import Types.Op as Op exposing (Binop, Unop)


type alias Function =
    { moduleName : String
    , localName : String
    , ty : Type
    , nonFn : List String
    }



----
--- Prelude
--


prelude : CA.AllDefs
prelude =
    Core.coreModule
        |> (\m -> Dict.foldl insertUnop m unops)
        |> (\m -> Dict.foldl insertBinop m binops)
        |> (\m -> List.foldl insertFunction m functions)


insertUnop : String -> Unop -> CA.AllDefs -> CA.AllDefs
insertUnop _ u =
    { name = u.symbol
    , localName = u.symbol
    , pos = pos
    , isNative = True
    , body = []
    , maybeAnnotation =
        Just
            { pos = pos
            , ty = u.ty
            , nonFn = Dict.empty
            }
    }
        |> CA.Value
        |> Dict.insert u.symbol


insertBinop : String -> Binop -> CA.AllDefs -> CA.AllDefs
insertBinop _ b =
    { name = b.symbol
    , localName = b.symbol
    , pos = pos
    , isNative = True
    , body = []
    , maybeAnnotation =
        Just
            { pos = pos
            , ty = b.ty
            , nonFn = List.foldl (\n -> Dict.insert n pos) Dict.empty b.nonFn
            }
    }
        |> CA.Value
        |> Dict.insert b.symbol


insertFunction : Function -> CA.AllDefs -> CA.AllDefs
insertFunction f =
    let
        name =
            f.moduleName ++ "." ++ f.localName
    in
    { name = name
    , localName = f.localName
    , pos = pos
    , isNative = True
    , body = []
    , maybeAnnotation =
        Just
            { pos = pos
            , ty = f.ty
            , nonFn = List.foldl (\n -> Dict.insert n pos) Dict.empty f.nonFn
            }
    }
        |> CA.Value
        |> Dict.insert name


pos : CA.Pos
pos =
    CA.N


tyVar : String -> Type
tyVar n =
    CA.TypeVariable pos n


tyFun from to =
    CA.TypeFunction pos from False to



----
--- Unops
--


unops : Dict String Unop
unops =
    [ unaryPlus
    , unaryMinus
    , not_
    ]
        |> List.foldl (\op -> Dict.insert op.symbol op) Dict.empty


typeUnopUniform : CA.Type -> CA.Type
typeUnopUniform ty =
    CA.TypeFunction pos ty False ty


unaryPlus : Unop
unaryPlus =
    { symbol = "0 +"
    , ty = typeUnopUniform Core.numberType
    }


unaryMinus : Unop
unaryMinus =
    { symbol = "0 -"
    , ty = typeUnopUniform Core.numberType
    }


not_ : Unop
not_ =
    { symbol = "not"
    , ty = typeUnopUniform Core.boolType
    }



----
--- Binops
--


binops : Dict String Binop
binops =
    [ textConcat
    , tuple
    , listCons
    , mutableAssign
    , and
    , or
    , add
    , subtract
    , multiply
    , divide
    , mutableAdd
    , mutableSubtract
    , equal
    , notEqual
    , lesserThan
    , greaterThan
    , sendRight
    , sendLeft
    ]
        |> List.foldl (\op -> Dict.insert op.symbol op) Dict.empty


typeBinop : Bool -> CA.Type -> CA.Type -> CA.Type -> CA.Type
typeBinop mutates left right return =
    CA.TypeFunction pos
        right
        False
        (CA.TypeFunction pos
            left
            mutates
            return
        )


typeBinopUniform ty =
    typeBinop False ty ty ty



-- Misc


textConcat : Binop
textConcat =
    { symbol = ".."
    , precedence = Op.Addittive
    , associativity = Op.Right
    , ty = typeBinopUniform Core.textType
    , nonFn = []
    }


tuple : Binop
tuple =
    { symbol = "&"
    , precedence = Op.Tuple
    , associativity = Op.NonAssociative
    , ty =
        Dict.empty
            |> Dict.insert "first" (tyVar "a")
            |> Dict.insert "second" (tyVar "b")
            |> CA.TypeRecord pos Nothing
            |> typeBinop False (tyVar "a") (tyVar "b")
    , nonFn = []
    }


listCons : Binop
listCons =
    let
        item =
            tyVar "item"
    in
    { symbol = "%"
    , precedence = Op.Cons
    , associativity = Op.Right
    , ty = typeBinop False item (Core.listType item) (Core.listType item)
    , nonFn = []
    }


mutableAssign : Binop
mutableAssign =
    { symbol = ":="
    , precedence = Op.Mutop
    , associativity = Op.Left
    , ty = typeBinop True (tyVar "a") (tyVar "a") Core.noneType
    , nonFn = [ "a" ]
    }



-- Arithmetic


and : Binop
and =
    { symbol = "and"
    , precedence = Op.Logical
    , associativity = Op.Right
    , ty = typeBinopUniform Core.boolType
    , nonFn = []
    }


or : Binop
or =
    { symbol = "or"
    , precedence = Op.Logical
    , associativity = Op.Right
    , ty = typeBinopUniform Core.boolType
    , nonFn = []
    }


add : Binop
add =
    { symbol = "+"
    , precedence = Op.Addittive
    , associativity = Op.Left
    , ty = typeBinopUniform Core.numberType
    , nonFn = []
    }


subtract : Binop
subtract =
    { symbol = "-"
    , precedence = Op.Addittive
    , associativity = Op.Left
    , ty = typeBinopUniform Core.numberType
    , nonFn = []
    }


multiply : Binop
multiply =
    { symbol = "*"
    , precedence = Op.Multiplicative
    , associativity = Op.Left
    , ty = typeBinopUniform Core.numberType
    , nonFn = []
    }


divide : Binop
divide =
    { symbol = "/"
    , precedence = Op.Multiplicative
    , associativity = Op.Left
    , ty = typeBinopUniform Core.numberType
    , nonFn = []
    }


mutableAdd : Binop
mutableAdd =
    { symbol = "+="
    , precedence = Op.Mutop
    , associativity = Op.NonAssociative
    , ty = typeBinop True Core.numberType Core.numberType Core.noneType
    , nonFn = []
    }


mutableSubtract : Binop
mutableSubtract =
    { symbol = "-="
    , precedence = Op.Mutop
    , associativity = Op.NonAssociative
    , ty = typeBinop True Core.numberType Core.numberType Core.noneType
    , nonFn = []
    }



-- Comparison


equal : Binop
equal =
    { symbol = "=="
    , precedence = Op.Comparison
    , associativity = Op.Left
    , ty = typeBinop False (tyVar "a") (tyVar "a") Core.boolType
    , nonFn = [ "a" ]
    }


notEqual : Binop
notEqual =
    { symbol = "/="
    , precedence = Op.Comparison
    , associativity = Op.Left
    , ty = typeBinop False (tyVar "a") (tyVar "a") Core.boolType
    , nonFn = [ "a" ]
    }


lesserThan : Binop
lesserThan =
    { symbol = "<"
    , precedence = Op.Comparison
    , associativity = Op.Left
    , ty = typeBinop False Core.numberType Core.numberType Core.boolType
    , nonFn = []
    }


greaterThan : Binop
greaterThan =
    { symbol = ">"
    , precedence = Op.Comparison
    , associativity = Op.Left
    , ty = typeBinop False Core.numberType Core.numberType Core.boolType
    , nonFn = []
    }


lesserOrEqual : Binop
lesserOrEqual =
    { symbol = "<="
    , precedence = Op.Comparison
    , associativity = Op.Left
    , ty = typeBinop False Core.numberType Core.numberType Core.boolType
    , nonFn = []
    }


greaterOrEqual : Binop
greaterOrEqual =
    { symbol = ">="
    , precedence = Op.Comparison
    , associativity = Op.Left
    , ty = typeBinop False Core.numberType Core.numberType Core.boolType
    , nonFn = []
    }



-- Pipes


sendRight : Binop
sendRight =
    { symbol = ">>"
    , precedence = Op.Pipe
    , associativity = Op.Left
    , ty =
        typeBinop False
            (tyVar "a")
            (tyFun (tyVar "a") (tyVar "b"))
            (tyVar "b")
    , nonFn = []
    }


sendLeft : Binop
sendLeft =
    { symbol = "<<"
    , precedence = Op.Pipe
    , associativity = Op.Right
    , ty =
        typeBinop False
            (tyFun (tyVar "a") (tyVar "b"))
            (tyVar "a")
            (tyVar "b")
    , nonFn = []
    }



----
--- Functions
--


functions : List Function
functions =
    [ debugTodo
    , debugLog
    , debugToHuman
    , basicsCompare
    ]


basicsCompare : Function
basicsCompare =
    { moduleName = "SPCore/Basics"
    , localName = "compare"
    , ty =
        tyFun (tyVar "a")
            (tyFun (tyVar "a")
                Core.numberType
            )
    , nonFn = [ "a" ]
    }



-- SPCore/Debug


debugTodo : Function
debugTodo =
    { moduleName = "SPCore/Debug"
    , localName = "todo"
    , ty = tyFun Core.textType (tyVar "a")
    , nonFn = []
    }


debugLog : Function
debugLog =
    { moduleName = "SPCore/Debug"
    , localName = "log"
    , ty =
        tyFun Core.textType
            (tyFun (tyVar "a") (tyVar "a"))
    , nonFn = []
    }


debugToHuman : Function
debugToHuman =
    { moduleName = "SPCore/Debug"
    , localName = "toHuman"
    , ty = tyFun (tyVar "a") Core.textType
    , nonFn = []
    }
