

alias Int =
    Number


identity as fn a: a =
    fn a:
    a


not as fn Bool: Bool =
    fn b:
    if b then False else True


applyIf as fn Bool, (fn a: a): fn a: a =
    fn condition, f:
    if condition then f else identity


# Am I actually using this?
btw as fn (fn a: b), a, c: c =
    fn f, a, c:
    f a
    c


cloneUni as fn @a: !a =
    fn @a:
    todo "cloneUni"


cloneImm as fn a: !a =
    fn a:
    todo "cloneImm"


#
# TODO move these to Number module?
#

max as fn Number, Number: Number =
    fn a, b:
    if a > b then a else b


min as fn Number, Number: Number =
    fn a, b:
    if a < b then a else b


clamp as fn Number, Number, Number: Number =
    fn low, high, n:
    if n < low then low
    else if n > high then high
    else n


round as fn Number: Int =
    fn n:
    todo "round"


modBy as fn Int, Int: Int =
    fn a, b:

    # Native
    todo "modBy is native"

