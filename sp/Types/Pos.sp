#
# The position of a piece of code
#
union Pos =
    # actual position: module reference, start, end
    , P Text Int Int
    # stripped
    , S
    # defined natively, usually in Core or Prelude
    , N
    # defined as test
    , T
    # inferred
    , I Int

    #  TODO the following ones need to be removed

    # error todo
    , E
    # Formattable to canonical todo
    , F
    # ScopeCheck HACK
    , G
    # Union
    , U


union At a =
    At Pos a


start pos =
    as Pos: Int
    try pos as
        P m s e: s
        _: 0


end pos =
    as Pos: Int
    try pos as
        P m s e: e
        _: 0


range a b =
    as Pos: Pos: Pos

    try a & b as
        P ma sa ea & P mb sb eb:
            if ma /= mb:
                todo "trying to range across two different modules"
            else
                P ma (min sa sb) (max ea eb)

        P _ _ _ & _:
            a

        _:
            b
