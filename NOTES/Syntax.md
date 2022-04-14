https://www.vidarholen.net/~vidar/An_Empirical_Investigation_into_Programming_Language_Syntax.pdf


Possible alternative syntax
===========================


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




# Callback operator


BIG problem: what if I forget a `None ?=`?
The statement is evaluated, but silently **not inserted in the monad chain**!!!!





If we use `=:` or `:=` for this instead of `?=` then we need to use something different for `mutable assign` (such as `=!` ?)
Or use `=!` for the callback op?
Or even `::`?


    meta :=
        loadMeta >> IO.onSuccess

    fileLists as List (Text & Text) ?=
        meta.sourceDirs
            >> List.map fn sd: listSourceDir sd.path
            >> IO.parallel
            >> IO.onSuccess

    modules as [CA.Module] ?=
        fileLists
            >> List.concat
            >> List.filter isSpFile
            >> List.map loadModule
            >> IO.parallel
            >> IO.onSuccess

    expanded ?=
        modules
            >> List.indexBy fn m: m.umr
            >> Compiler/Pipeline.globalExpandedTypes
            >> onResSuccess

    errorsAndEnvs ?=
        modules
            >> List.map typeCheckModule
            >> IO.parallel
            >> IO.onSuccess

    # TODO emit js

    IO.return None



+ much nicer to read and write
- more difficult to figure out what is happening

---> Instead of showing what happens on success, it might be a lot more clear to make explicit what happens on *error*
    (ie, when the callback is NOT called!)



    meta ?=
        loadMeta >> IO.breakOnError

    fileLists as List (Text & Text) ?=
        meta.sourceDirs
            >> List.map fn sd: listSourceDir sd.path
            >> IO.parallel
            >> IO.breakOnError

    modules as [CA.Module] ?=
        fileLists
            >> List.concat
            >> List.filter isSpFile
            >> List.map loadModule
            >> IO.parallel
            >> IO.breakOnError

    expanded ?=
        modules
            >> List.indexBy fn m: m.umr
            >> Compiler/Pipeline.globalExpandedTypes
            >> breakOnResError

    errorsAndEnvs ?=
        modules
            >> List.map typeCheckModule
            >> IO.parallel
            >> IO.breakOnError

    # TODO emit js

    IO.return None


+ easier to follow the flow
- more verbose




Things that are worth considering but need thinking
---------------------------------------------------

# Add notation to extend records

  { extend someRecord with someAttribute = 1, someOtherAttribute = 2 }

  If someRecord already has someAttribute or someOtherAttribute, the type checker should produce an error.
