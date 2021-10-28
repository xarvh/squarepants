
alias Int =
    Number


#
# Helpers
#

[#
typeBinop mutates left right return =
    is Bool -> CA.Type -> CA.Type -> CA.Type -> CA.Type
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
    is Dict Text Op.Binop
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
    is Op.Unop
    { symbol = "0 +"
#    , ty = typeUnopUniform Core.numberType
    }


unaryMinus =
    is Op.Unop
    { symbol = "0 -"
#    , ty = typeUnopUniform Core.numberType
    }


not_ =
    is Op.Unop
    { symbol = "not"
#    , ty = typeUnopUniform Core.boolType
    }


and_ =
    is Op.Binop
    { symbol = "and"
    , precedence = Op.Logical
    , associativity = Op.Right
#    , ty = typeBinopUniform Core.boolType
    , nonFn = []
    }


or_ =
    is Op.Binop
    { symbol = "or"
    , precedence = Op.Logical
    , associativity = Op.Right
#    , ty = typeBinopUniform Core.boolType
    , nonFn = []
    }

textConcat =
    is Op.Binop
    { symbol = ".."
    , precedence = Op.Addittive
    , associativity = Op.Right
#    , ty = typeBinopUniform Core.textType
    , nonFn = []
    }


add =
    is Op.Binop
    { symbol = "+"
    , precedence = Op.Addittive
    , associativity = Op.Left
#    , ty = typeBinopUniform Core.numberType
    , nonFn = []
    }


subtract =
    is Op.Binop
    { symbol = "-"
    , precedence = Op.Addittive
    , associativity = Op.Left
#    , ty = typeBinopUniform Core.numberType
    , nonFn = []
    }


multiply =
    is Op.Binop
    { symbol = "*"
    , precedence = Op.Multiplicative
    , associativity = Op.Left
#    , ty = typeBinopUniform Core.numberType
    , nonFn = []
    }


divide =
    is Op.Binop
    { symbol = "/"
    , precedence = Op.Multiplicative
    , associativity = Op.Left
#    , ty = typeBinopUniform Core.numberType
    , nonFn = []
    }


mutableAdd =
    is Op.Binop
    { symbol = "+="
    , precedence = Op.Mutop
    , associativity = Op.NonAssociative
#    , ty = typeBinop True Core.numberType Core.numberType Core.noneType
    , nonFn = []
    }


mutableSubtract =
    is Op.Binop
    { symbol = "-="
    , precedence = Op.Mutop
    , associativity = Op.NonAssociative
#    , ty = typeBinop True Core.numberType Core.numberType Core.noneType
    , nonFn = []
    }
