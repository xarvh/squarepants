
alias Int =
    Number


identity as a: a =
    a:
    a


not as Bool: Bool =
    b:
    if b then False else True


max as Number: Number: Number =
    a: b:
    if a > b then a else: b


min as Number: Number: Number =
    a: b:
    if a < b then a else: b


clamp as Number: Number: Number: Number =
    low: high: n:
    if n < low then low
    else if n > high then high
    else n


modBy as Int: Int: Int =
    a: b:

    # Native
    todo "modBy is native"

