#
# The position of a piece of code
#
# The names are kept very short to keep them debug-output-friendly
#
var Pos =
    , # actual position: start, end
      'p Int Int
    , # module-wide error
      'm
    , # parsing reached the end of file
      'end
    , # stripped
      's
    , # defined natively, usually in Core or Prelude
      'n
    , # defined as test
      't
    , # inferred
      'i Int
    , # generated
      'g


# TODO this was not super useful, remove it
var At a =
    , 'at Pos a


start as fn Pos: Int =
    fn pos:
    try pos as
        'p s e: s
        _: 0


end as fn Pos: Int =
    fn pos:
    try pos as
        'p s e: e
        _: 0


range as fn Pos, Pos: Pos =
    fn a, b:
    try a & b as
        'p sa ea & 'p sb eb: 'p (min sa sb) (max ea eb)
        'p _ _ & _: a
        _: b


drop as fn At a: a =
    fn x:
    'at pos a =
        x

    a
