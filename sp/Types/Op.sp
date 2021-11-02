

alias Unop =
    {
    , symbol as Text
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
    , precedence as Op.Precedence
    , associativity as Op.Associativity
    , symbol as Text
    #, ty as CA.Type
    , nonFn as [Text]
    }
