#
# The position of a piece of code
#
# The names are kept very short to keep them debug-output-friendly
#
union Pos =
    # actual position: start, end
    , P Int Int
    # module-wide error
    , M
    # parsing reached the end of file
    , End
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


start as fn Pos: Int =
    fn pos:
    try pos as
        , P s e: s
        , _: 0


end as fn Pos: Int =
    fn pos:
    try pos as
        , P s e: e
        , _: 0


range as fn Pos, Pos: Pos =
    fn a, b:
    try a & b as
        , P sa ea & P sb eb:
            P (min sa sb) (max ea eb)

        , P _ _ & _:
            a

        , _:
            b

