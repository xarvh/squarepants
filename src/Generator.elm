module Generator exposing (..)


type Generator state output
    = Generator (state -> ( output, state ))


{-| monadic return
-}
wrap : output -> Generator state output
wrap v =
    Generator (\state -> ( v, state ))


next : (state -> state) -> (state -> output) -> Generator state output
next nextState stateToOutput =
    Generator (\state -> ( stateToOutput state, nextState state ))


map : (a -> b) -> Generator state a -> Generator state b
map f (Generator genA) =
    Generator (genA >> Tuple.mapFirst f)


do : Generator state a -> (a -> Generator state b) -> Generator state b
do (Generator genA) f =
    Generator
        (\state ->
            let
                ( result, newState ) =
                    genA state

                (Generator genB) =
                    f result
            in
            genB newState
        )


run : state -> Generator state output -> ( output, state )
run s (Generator a) =
    a s
