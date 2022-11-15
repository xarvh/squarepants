

alias Unop = {
    , symbol as Text
    , type as CA.Type
    , usr as Meta.UniqueSymbolReference
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


alias Binop = {
    , precedence as Op.Precedence
    , associativity as Op.Associativity
    , symbol as Text
    , type as CA.Type
    , usr as Meta.UniqueSymbolReference
    , nonFn as [Text]
    }
