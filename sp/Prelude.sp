
alias Int =
    Number


#
# Helpers
#

[#
typeBinop mutates left right return =
    as Bool: CA.Type: CA.Type: CA.Type: CA.Type
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
#]



#
# Unops
#
not_ =
    as Op.Unop
    { symbol = "not"
#    , ty = typeUnopUniform Core.boolType
    }

unaryPlus =
    as Op.Unop
    { symbol = "0 +"
#    , ty = typeUnopUniform Core.numberType
    }

unaryMinus =
    as Op.Unop
    { symbol = "0 -"
#    , ty = typeUnopUniform Core.numberType
    }


#
# Binops
#
binops =
    as Dict Text Op.Binop
    [
    , and_
    , or_
    , textConcat
    , listCons
    , tuple
    #
    , add
    , subtract
    , multiply
    , divide
    #
    , mutableAssign
    , mutableAdd
    , mutableSubtract
    #
    , equal
    , notEqual
    , lesserThan
    , greaterThan
    , lesserOrEqualThan
    , greaterOrEqualThan
    #
    , sendRight
    , sendLeft
    ]
      >> (fn list: List.foldl (fn bop: Dict.insert bop.symbol bop) list Dict.empty)



#
# Core types ops
#
and_ =
    as Op.Binop
    { symbol = "and"
    , precedence = Op.Logical
    , associativity = Op.Right
#    , ty = typeBinopUniform Core.boolType
    , nonFn = []
    }


or_ =
    as Op.Binop
    { symbol = "or"
    , precedence = Op.Logical
    , associativity = Op.Right
#    , ty = typeBinopUniform Core.boolType
    , nonFn = []
    }

textConcat =
    as Op.Binop
    { symbol = ".."
    , precedence = Op.Addittive
    , associativity = Op.Right
#    , ty = typeBinopUniform Core.textType
    , nonFn = []
    }


listCons =
    as Op.Binop
#    item = tyVar "item"
    { symbol = "::"
    , precedence = Op.Cons
    , associativity = Op.Right
#    , ty = typeBinop False item (Core.listType item) (Core.listType item)
    , nonFn = []
    }


tuple =
    as Op.Binop
    { symbol = "&"
    , precedence = Op.Tuple
    , associativity = Op.NonAssociative
#    , ty =
#        Dict.empty
#            |> Dict.insert "first" (tyVar "a")
#            |> Dict.insert "second" (tyVar "b")
#            |> CA.TypeRecord pos Nothing
#            |> typeBinop False (tyVar "a") (tyVar "b")
    , nonFn = []
    }



#
# Arithmetic ops
#
add =
    as Op.Binop
    { symbol = "+"
    , precedence = Op.Addittive
    , associativity = Op.Left
#    , ty = typeBinopUniform Core.numberType
    , nonFn = []
    }


subtract =
    as Op.Binop
    { symbol = "-"
    , precedence = Op.Addittive
    , associativity = Op.Left
#    , ty = typeBinopUniform Core.numberType
    , nonFn = []
    }


multiply =
    as Op.Binop
    { symbol = "*"
    , precedence = Op.Multiplicative
    , associativity = Op.Left
#    , ty = typeBinopUniform Core.numberType
    , nonFn = []
    }


divide =
    as Op.Binop
    { symbol = "/"
    , precedence = Op.Multiplicative
    , associativity = Op.Left
#    , ty = typeBinopUniform Core.numberType
    , nonFn = []
    }


#
# Mut ops
#
mutableAssign =
    as Op.Binop
    {
    , symbol = ":="
    , precedence = Op.Mutop
    , associativity = Op.Left
#    , ty = typeBinop True (tyVar "a") (tyVar "a") Core.noneType
    , nonFn = []
    }


mutableAdd =
    as Op.Binop
    { symbol = "+="
    , precedence = Op.Mutop
    , associativity = Op.NonAssociative
#    , ty = typeBinop True Core.numberType Core.numberType Core.noneType
    , nonFn = []
    }


mutableSubtract =
    as Op.Binop
    { symbol = "-="
    , precedence = Op.Mutop
    , associativity = Op.NonAssociative
#    , ty = typeBinop True Core.numberType Core.numberType Core.noneType
    , nonFn = []
    }


#
# Comparison
#
equal =
    as Op.Binop
    { symbol = "=="
    , precedence = Op.Comparison
    , associativity = Op.Left
#    , ty = typeBinop False (tyVar "a") (tyVar "a") Core.boolType
    , nonFn = [ "a" ]
    }


notEqual =
    as Op.Binop
    { symbol = "/="
    , precedence = Op.Comparison
    , associativity = Op.Left
#    , ty = typeBinop False (tyVar "a") (tyVar "a") Core.boolType
    , nonFn = [ "a" ]
    }


lesserThan =
    as Op.Binop
    { symbol = "<"
    , precedence = Op.Comparison
    , associativity = Op.Left
#    , ty = typeBinop False (tyVar "a") (tyVar "a") Core.boolType
    , nonFn = [ "a" ]
    }


greaterThan =
    as Op.Binop
    { symbol = ">"
    , precedence = Op.Comparison
    , associativity = Op.Left
#    , ty = typeBinop False (tyVar "a") (tyVar "a") Core.boolType
    , nonFn = [ "a" ]
    }


lesserOrEqualThan =
    as Op.Binop
    { symbol = "<="
    , precedence = Op.Comparison
    , associativity = Op.Left
#    , ty = typeBinop False (tyVar "a") (tyVar "a") Core.boolType
    , nonFn = [ "a" ]
    }


greaterOrEqualThan =
    as Op.Binop
    { symbol = ">="
    , precedence = Op.Comparison
    , associativity = Op.Left
#    , ty = typeBinop False (tyVar "a") (tyVar "a") Core.boolType
    , nonFn = [ "a" ]
    }



#
# Send
#
sendRight =
    as Op.Binop
    {
    , symbol = ">>"
    , precedence = Op.Pipe
    , associativity = Op.Left
#    , ty =
#        typeBinop False
#            (tyVar "a")
#            (tyFun (tyVar "a") (tyVar "b"))
#            (tyVar "b")
    , nonFn = []
    }


sendLeft =
    as Op.Binop
    {
    , symbol = "<<"
    , precedence = Op.Pipe
    , associativity = Op.Right
#    , ty =
#        typeBinop False
#            (tyFun (tyVar "a") (tyVar "b"))
#            (tyVar "a")
#            (tyVar "b")
    , nonFn = []
    }

