alias Int =
    Number


# TODO
binops =
    as Dict Text binop
    Dict.empty



unaryPlus =
    as Types/Unop.Unop
    { symbol = "0 +"
#    , ty = typeUnopUniform Core.numberType
    }


unaryMinus =
    as Types/Unop.Unop
    { symbol = "0 -"
#    , ty = typeUnopUniform Core.numberType
    }


not_ =
    as Types/Unop.Unop
    { symbol = "not"
#    , ty = typeUnopUniform Core.boolType
    }


and_ =
    as Types/Binop.Binop
    { symbol = "and"
#    , precedence = Op.Logical
#    , associativity = Op.Right
#    , ty = typeBinopUniform Core.boolType
#    , nonFn = []
    }


or_ =
    as Types/Binop.Binop
    { symbol = "or"
#    , precedence = Op.Logical
#    , associativity = Op.Right
#    , ty = typeBinopUniform Core.boolType
#    , nonFn = []
    }

textConcat =
    as Types/Binop.Binop
    { symbol = ".."
#    , precedence = Op.Addittive
#    , associativity = Op.Right
#    , ty = typeBinopUniform Core.textType
#    , nonFn = []
    }
