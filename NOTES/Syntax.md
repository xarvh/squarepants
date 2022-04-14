Possible alternative syntax
===========================


Things that should probably be implemented
------------------------------------------

# List packing instead of cons op

  use

        [ head, ...tail ]

  in place of

        head :: tail


# Callback operator

Problem: this sucks:

    ```
    xxx =
        valueType >> list_for patternsAndBlocks (pattern & block): typeSoFar:
            fromPattern env pattern Dict.empty >> andThen patternOut:
            unify patternOut.type typeSoFar

    xxx >> andThen unifiedValueType:

    doStuffWith unifiedValueType
    ```


Solution:

    ```
    unifiedValueType =:
        valueType
            >> list_for patternsAndBlocks (pattern & block): typeSoFar:
                patternOut =:
                    fromPattern env pattern Dict.empty >> stateCallback

                unify patternOut.type typeSoFar
            >> stateCallback

    doStuffWith unifiedValueType
    ```


    --> However, consider:

    ```
    None :=
        None >> dict_for attrValueByName attrName: attrValue: None:
            try Dict.get attrName attrTypeByName as
                Nothing:
                    error << "missing attribute " .. attrName

                Just expectedAttrType:
                    checkExpression env attrType attrValue

    doOtherStuffInsideTheMonad
    ```

    Is it obvious enough why we're using `:=`?
    Is it obvious enough that this is a continuation?



If we use `=:` or `:=` for this instead of `?=` then we need to use something different for `mutable assign` (such as `=!` ?)
Or use `=!` for the callback op?


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



# I don't like how mutation is annotated

    Prefixing the @ requires more parens.

    Int: (@ Maybe Int: Int): Int

    Int -> (Maybe Int @> Int) -> Int


Obsolete
--------


# Distinguish `as` in `try..as` from `as` in annotations

  * try..as -> match..to
  * try..as -> try..on
  * if value like


# Use `:` instead of `=`

    a_variable:
        3

    a_function: fn x y z:
        x + y + z

    alias A_Record_Type:
        { x: Int, y: Int }

    a_record_value:
        { x: 1, y: 2 }

- most languages use `=`
+ record type and value definitions are identical
+ in line with math notation that uses `:=` as defop
+ `:` is more consistently used for /defining/ stuff, rather than making statements of truth
+ `:` is more accessible than `=`
- `:` requires a Shift on an US keyboard (but `=` also does on most non-US keyboards)
+ `:` can be used without a leading space

In theory we'd have also:
+ can use `=` instead of `==` for comparison
+ removes the ambiguous use of the `=` symbol

* Use `!=`? (Which does not mean necessarily that we use unary `!`)

* But mutable numerical ops `+=` `-=` `*=` `/=` still use `=`
(Maybe I should just dump them, or use something different? Or just keep `+=` and hope no one notices?)
Also, what do we use for mutable assignment?
  Options assuming that mutation is marked:

    @x <- 4

  Options assuming that mutation is not marked:

    x @ 4
    x <- 4

  Even worse, generalize mutops:

    x +@ 4
    x *@ 4
    x ::@ 4

    @{binop} a b: a @ (a binop b)


# do-notation

### Version 1, no new syntax

    onOk = Result.onOk
    blah blah blah >> onOk blahOutput:
    someotherline >> onOk otherThingy:
    doStuffWith blahOutput otherThingy

+ Uses known syntax
+ Reading is enough to figure out exactly what is happening
- Forces the eyes to parse the end of the line to get the new variable name
- Doesn't look nice with multi-line statements
- A lot more dense than normal declarations
- Difficult to read at glance


### Version 2: ad-hoc =? syntax

    onOk =
        Result.onOk

    blahOutput =?
        onOk << blah blah blah

    otherThingy =?
        newVariable = blah
        someotherline newVariable
            >> onOk

    doStuffWith blahOutput otherThingy

+ More intuitive
+ Plays well with the rest of the style
- Adds more syntax
- More confusing to understand what is actually happening

    functionArgument ?=
        (something that gets the function as argument)

    functionBody







# Use `:` instead of `then` in if statements

  * if expr: blah1 else: blah2

  * if expr: blah1 else blah2

  -> Both `else` and `else:` are valid
  -> auto-formatting will turn `else` into `else:`

  + This has the advantage that `then` is not a keyword any more
  - `else:` is super ugly

