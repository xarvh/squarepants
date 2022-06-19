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
    # generated
    , G


union At a =
    At Pos a


start as Pos: Int =
    pos:
    try pos as
        P m s e: s
        _: 0


end as Pos: Int =
    pos:
    try pos as
        P m s e: e
        _: 0


range as Pos: Pos: Pos =
    a: b:
    try a & b as
        P ma sa ea & P mb sb eb:
            if ma /= mb then
                todo "trying to range across two different modules"
            else
                P ma (min sa sb) (max ea eb)

        P _ _ _ & _:
            a

        _:
            b


drop as At a: a =
    x:
    At pos a = x
    a

