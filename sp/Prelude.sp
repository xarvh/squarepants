alias Int =
    Number


# TODO
binops =
    is Dict Text binop
    Dict.empty



unaryPlus =
    is Types/Unop.Unop
    { symbol = "0 +"
#    , ty = typeUnopUniform Core.numberType
    }


unaryMinus =
    is Types/Unop.Unop
    { symbol = "0 -"
#    , ty = typeUnopUniform Core.numberType
    }


not_ =
    is Types/Unop.Unop
    { symbol = "not"
#    , ty = typeUnopUniform Core.boolType
    }


and_ =
    is Types/Binop.Binop
    { symbol = "and"
#    , precedence = Op.Logical
#    , associativity = Op.Right
#    , ty = typeBinopUniform Core.boolType
#    , nonFn = []
    }


or_ =
    is Types/Binop.Binop
    { symbol = "or"
#    , precedence = Op.Logical
#    , associativity = Op.Right
#    , ty = typeBinopUniform Core.boolType
#    , nonFn = []
    }

textConcat =
    is Types/Binop.Binop
    { symbol = ".."
#    , precedence = Op.Addittive
#    , associativity = Op.Right
#    , ty = typeBinopUniform Core.textType
#    , nonFn = []
    }
