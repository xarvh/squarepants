#
# TODO I don't know yet how I want to implement fast sequential access
# so for now I'll jusr wrap everything in this module to hide the implementation
#
alias Buffer =
    {
    , nextPos as Int
    , fullSize as Int
    , fullText as Text
    }


init s =
    as Text: Buffer

    {
    , nextPos = 0
    , fullSize = Text.length s
    , fullText = s
    }


readOne b =
    as Buffer: Text & Buffer

    if b.nextPos < b.fullSize:
      Text.slice b.nextPos (b.nextPos + 1) b.fullText & { b with nextPos = .nextPos + 1 }
    else
      "" & b


slice start end b =
    as Int: Int: Buffer: Text

    Text.slice start end b.fullText
