https://www.vidarholen.net/~vidar/An_Empirical_Investigation_into_Programming_Language_Syntax.pdf


Things that should probably be implemented
------------------------------------------

# List packing instead of cons op

  use

        [ head, ...tail ]

  in place of

        head :: tail


# Partial record unpacking

  { someAttribute, ... }

  { with someAttribute }





Things that are worth considering but need thinking
---------------------------------------------------

# Allows statements to start with an operator

    1 + 3 * 3 - 1
    ---> 1 + 3 * 3 - 1


    1 + 3 *
    3 - 1
    ---> Error (because it's not readable)


    1 + 3
    * 3 - 1
    ---> (1 + 3) * 3 - 1


    1 + 3
    *
    3 - 1
    ----> (1 + 3) * (3 - 1)




  This allows to express do-notation nicely:

    try blah as
        Nothing: ...
        Just b: ...
    >> andThen _:


multi line function call must be indented

    function
        arg1
        arg2
        >> ...

    -------> error


block indent should be an interval (in particular for parens?)










# Add notation to extend records

  { extend someRecord with someAttribute = 1, someOtherAttribute = 2 }

  If someRecord already has someAttribute or someOtherAttribute, the type checker should produce an error.
