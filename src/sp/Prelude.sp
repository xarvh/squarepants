
alias Int =
    Number


#
# Helpers
#

tyVar as Name: CA.Type =
    CA.TypeVariable Pos.N


tyFun as CA.Type: Bool: CA.Type: CA.Type =
    CA.TypeFunction Pos.N


typeUnopUniform as CA.Type: CA.Type =
    type:
    tyFun type False type


typeBinop as Bool: CA.Type: CA.Type: CA.Type: CA.Type =
    mutates: left: right: return:
    tyFun
        right
        False
        (tyFun
            left
            mutates
            return
        )


typeBinopUniform as CA.Type: CA.Type =
    ty:
    typeBinop False ty ty ty



#
# Unops
#
not_ as Op.Unop = {
    , symbol = "not"
    , type = typeUnopUniform CoreTypes.bool
    }

unaryPlus as Op.Unop = {
    , symbol = "0 +"
    , type = typeUnopUniform CoreTypes.number
    }

unaryMinus as Op.Unop = {
    , symbol = "0 -"
    , type = typeUnopUniform CoreTypes.number
    }


#
# Binops
#
binops as Dict Text Op.Binop =
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
#    , mutableAdd
#    , mutableSubtract
    #
#    , equal
#    , notEqual
#    , lesserThan
#    , greaterThan
#    , lesserOrEqualThan
#    , greaterOrEqualThan
    #
#    , sendRight
#    , sendLeft
    ]
      >> (list: List.foldl (bop: Dict.insert bop.symbol bop) list Dict.empty)



#
# Core types ops
#
and_ as Op.Binop =
    { symbol = "and"
    , precedence = Op.Logical
    , associativity = Op.Right
    , type = typeBinopUniform CoreTypes.bool
    , nonFn = []
    }


or_ as Op.Binop =
    { symbol = "or"
    , precedence = Op.Logical
    , associativity = Op.Right
    , type = typeBinopUniform CoreTypes.bool
    , nonFn = []
    }

textConcat as Op.Binop =
    { symbol = ".."
    , precedence = Op.Addittive
    , associativity = Op.Right
    , type = typeBinopUniform CoreTypes.text
    , nonFn = []
    }


listCons as Op.Binop =
    item =
        tyVar "item"

    { symbol = "::"
    , precedence = Op.Cons
    , associativity = Op.Right
    , type = typeBinop False item (CoreTypes.list item) (CoreTypes.list item)
    , nonFn = []
    }


tuple as Op.Binop =
    { symbol = "&"
    , precedence = Op.Tuple
    , associativity = Op.NonAssociative
    , type =
        Dict.empty
            >> Dict.insert "first" (tyVar "a")
            >> Dict.insert "second" (tyVar "b")
            >> CA.TypeRecord Pos.N Nothing
            >> typeBinop False (tyVar "a") (tyVar "b")
    , nonFn = []
    }



#
# Arithmetic ops
#
add as Op.Binop =
    { symbol = "+"
    , precedence = Op.Addittive
    , associativity = Op.Left
    , type = typeBinopUniform CoreTypes.number
    , nonFn = []
    }


subtract as Op.Binop =
    { symbol = "-"
    , precedence = Op.Addittive
    , associativity = Op.Left
    , type = typeBinopUniform CoreTypes.number
    , nonFn = []
    }


multiply as Op.Binop =
    { symbol = "*"
    , precedence = Op.Multiplicative
    , associativity = Op.Left
    , type = typeBinopUniform CoreTypes.number
    , nonFn = []
    }


divide as Op.Binop =
    { symbol = "/"
    , precedence = Op.Multiplicative
    , associativity = Op.Left
    , type = typeBinopUniform CoreTypes.number
    , nonFn = []
    }


#
# Mut ops
#
mutableAssign as Op.Binop = {
    , symbol = ":="
    , precedence = Op.Mutop
    , associativity = Op.Left
    , type = typeBinop True (tyVar "a") (tyVar "a") CoreTypes.none
    , nonFn = []
    }

