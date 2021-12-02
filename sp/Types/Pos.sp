#
# The position of a piece of code
#
# The names are kept very short to keep them debug-output-friendly
#
union Pos =
    # actual position: module reference, start, end
    , P Text Int Int
    # parsing reched the end of file
    , End Text
    # stripped
    , S
    # defined natively, usually in Core or Prelude
    , N
    # defined as test
    , T
    # inferred
    , I Int


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


drop (At pos a) =
    as At a: a
    a

