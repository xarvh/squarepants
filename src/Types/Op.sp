
union UnopId =
    , UnopPlus
    , UnopMinus
    , UnopUnique
    , UnopRecycle


alias Unop =
    {
    , symbol as Text
    , type as CA.RawType
    , usr as USR
    }


union Associativity =
    , NonAssociative
    , Left
    , Right


alias Binop =
    {
    , precedence as Int
    # TODO: we don't use this at all. Do we still need it? Are ops associated correctly?
    , associativity as Op.Associativity
    , symbol as Text
    , type as CA.RawType
    , usr as USR
    , nonFn as [Text]
    }


precedence_function as Int = 0
precedence_application as Int = 9
precedence_multiplicative as Int = 8
precedence_addittive as Int = 7
precedence_comparison as Int = 6
precedence_logical as Int = 5
precedence_tuple as Int = 4
precedence_cons as Int = 3
precedence_pipe as Int = 2
precedence_mutop as Int = 1

