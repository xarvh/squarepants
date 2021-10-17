

alias Unop =
    {
    , symbol is Text
    }


union Associativity =
    , NonAssociative
    , Left
    , Right


union Precedence =
    , Exponential
    , Multiplicative
    , Addittive
    , Cons
    , Comparison
    , Logical
    , Tuple
    , Pipe
    , Mutop


alias Binop =
    {
    , precedence is Op.Precedence
    , associativity is Op.Associativity
    , symbol is Text
    #, ty is CA.Type
    , nonFn is [Text]
    }
