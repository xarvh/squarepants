
[# My understanding of monads makes me think that this is some sort of state monad.
Because it has state.
But I don't know, Haskell explanations are still impenetrable to me.
#]



[# I could wrap the definition inside an opaque type, but I don't think the benefits are worth the extra fiddling

(Which is why Squarepants allows opaque types only inside libraries: you should use one only when you have a complete idea of how it will be used,
otherwise you just end up with wrap/unwrap helpers which moot the whole point of opacity...)

#]
alias M state a =
    state: ( a & state )


andThen as (a: M state b): M state a: M state b =
    f: m: state0:

    a & state1 =
        m state0

    f a state1


return as a: M state a =
    a: state:
    a & state


update as (state: state): M state state =
    f: state:

    s =
        f state

    s & s


run as state: M state a: a & state =
    state: m:
    m state


get as (state: a): M state a =
    getter: state:
    getter state & state





map as (a: b): M state a: M state b =
    f: m:
    andThen (x: x >> f >> return) m



# Maybe


maybe_map as (a: M state b): Maybe a: M state (Maybe b) =
    f: ma:
    try ma as
        Nothing:
            return Nothing

        Just a:
            f a >> andThen b:
            return << Just b



# List


list_foldl as (item: accum: M state accum): List item: accum: M state accum =
    f: items: accum:
    try items as
        []:
            return accum

        head :: tail:
            andThen (list_foldl f tail) (f head accum)


list_map as (a: M state b): List a: M state (List b) =
    f: la:

    apply = a: accum:
        f a >> andThen b:
        return (b :: accum)

    andThen (x: x >> List.reverse >> return) (list_foldl apply la [])


list_map2 as (a: b: M state c): List a: List b: M state (List c) =
    f: la: lb:

    apply = ( a & b ): accum:
        f a b >> andThen c:
        return (c :: accum)

    andThen (x: x >> List.reverse >> return) (list_foldl apply (List.map2 Tuple.pair la lb) [])



# Dict


dict_foldl as (comparable: item: accum: M state accum): Dict comparable item: accum: M state accum =
    f: x:
    x >> Dict.toList >> list_foldl (( k & v ): f k v)


dict_map as (comparable: a: M state b): Dict comparable a: M state (Dict comparable b) =
    f: dict:

    insert as comparable: a: Dict comparable b: M state (Dict comparable b) =
        c: a: d:
        f c a >> andThen b:
        return (Dict.insert c b d)

    dict_foldl insert dict Dict.empty
