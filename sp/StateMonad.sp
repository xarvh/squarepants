
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


then f m state0 =
    as (a: M state b): M state a: M state b

    a & state1 =
        m state0

    f a state1


return a state =
    as a: M state a
    a & state


update f state =
    as (state: state): M state state

    s =
        f state

    s & s


run state m =
    as state: M state a: a & state
    m state


get getter state =
    as (state: a): M state a
    getter state & state





map f m =
    as (a: b): M state a: M state b
    then (fn x: x >> f >> return) m



# Maybe


maybe_map f ma =
    as (a: M state b): Maybe a: M state (Maybe b)
    try ma as
        Nothing:
            return Nothing

        Just a:
            f a >> then fn b:
            return << Just b



# List


list_foldl f items accum =
    as (item: accum: M state accum): List item: accum: M state accum
    try items as
        []:
            return accum

        head :: tail:
            then (list_foldl f tail) (f head accum)


list_map f la =
    as (a: M state b): List a: M state (List b)

    apply a accum =
        f a >> then fn b:
        return (b :: accum)

    then (fn x: x >> List.reverse >> return) (list_foldl apply la [])


list_map2 f la lb =
    as (a: b: M state c): List a: List b: M state (List c)

    apply ( a & b ) accum =
        f a b >> then fn c:
        return (c :: accum)

    then (fn x: x >> List.reverse >> return) (list_foldl apply (List.map2 Tuple.pair la lb) [])



# Dict


dict_foldl f x =
    as (comparable: item: accum: M state accum): Dict comparable item: accum: M state accum
    x >> Dict.toList >> list_foldl (fn ( k & v ): f k v)


dict_map f dict =
    as (comparable: a: M state b): Dict comparable a: M state (Dict comparable b)

    insert c a d =
        as comparable: a: Dict comparable b: M state (Dict comparable b)
        f c a >> then fn b:
        return (Dict.insert c b d)

    dict_foldl insert dict Dict.empty
