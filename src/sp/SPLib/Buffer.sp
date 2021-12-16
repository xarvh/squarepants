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


init as Text: Buffer =
    s:
    {
    , nextPos = 0
    , fullSize = Text.length s
    , fullText = s
    }


readOne as Buffer: Text & Buffer =
    b:
    if b.nextPos < b.fullSize then
      Text.slice b.nextPos (b.nextPos + 1) b.fullText & { b with nextPos = .nextPos + 1 }
    else
      "" & b


slice as Int: Int: Buffer: Text =
    start: end: b:
    Text.slice start end b.fullText
