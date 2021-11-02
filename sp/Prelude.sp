
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



binops =
    as Dict Text Op.Binop
    [
    , and_
    , or_
    , textConcat
    , subtract
    , multiply
    , divide
    , mutableAdd
    , mutableSubtract
    ]
      >> (fn list: List.foldl (fn bop: Dict.insert bop.symbol bop) list Dict.empty)


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


not_ =
    as Op.Unop
    { symbol = "not"
#    , ty = typeUnopUniform Core.boolType
    }


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
