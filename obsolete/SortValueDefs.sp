[#


This module finds the correct initialization order of all root values.



Functions that return functions
-------------------------------


    f =
        List.for []

Assuming we don't know that List.for takes 3 parameters, the only rankedDeps we can produce is:

    [ Dict.singleton (USR List.for) 1 ]

This will ignore that `List.for` may have dependencies beyond rank 1.

How do we solve it?

  * Track down how `f` is used, so we can increase the rank for (USR List.for) accordingly
    (And we need to do this also for lambda arguments!)
    This is the accurate approach (which may or may not work for records/unions that contain functions?)

  * Make the type available, assume that at some point the function is called totally,
    and just add all the ranks to the whole ranked deps, without distinguishing
    between the dependencies of local defs and those of the return expression.

  * Squash all ranks of all deps to rank 0?

The first option may not be super efficient, but should be the most reliable, so let's go for that one.



Arguments that are functions
----------------------------

We just assume that the whole argument is called.

This means, we take all the ranks of the argument and we collapse them to rank 0.

#]


# Dependencies for a single rank
alias RankDeps =
    Dict Meta.UniqueSymbolReference Int


join as RankDeps: RankDeps: RankDeps =
    a: b:

    Dict.merge
        Dict.insert
        (usr: rankA: rankB: Dict.insert usr (max rankA rankB))
        Dict.insert
        a
        b
        Dict.empty



merge as [RankDeps]: [RankDeps]: [RankDeps] =

    rec as [RankDeps]: [RankDeps]: [RankDeps]: [RankDeps] =
        acc: a: b:

        try a & b as
            [] & []:
                List.reverse acc

            _ & []:
                List.append a (List.reverse acc)

            [] & _:
                List.append b (List.reverse acc)

            (a_head :: a_tail) & (b_head :: b_tail):
                rec (join a_head b_head :: acc) a_tail b_tail

    rec []


collapse as [RankDeps]: RankDeps =
    ranks:

    Dict.empty
        >> List.for ranks join




sortValueDefs as [CA.Module]: Result [Meta.UniqueSymbolReference] [CA.ValueDef] =
    modules:

    allDefsByUsr as Dict Meta.UniqueSymbolReference CA.ValueDef =
        Dict.empty >>
            List.for modules module:
                Dict.for module.valueDefs _: def:
                    Dict.insert (makeUsr module def) def


    #reorder as (node: Set key): Dict key node: Result [key] [node] with key NonFunction =


    getNodeDependencies as CA.ValueDef: Set Meta.UniqueSymbolReference =
      SPCore.todo ""

    RefHierarchy.reorder
      getNodeDependencies
      allDefsByUsr






makeUsr as CA.Module: CA.ValueDef: Meta.UniqueSymbolReference =
    module: def:

    try def.pattern as
      CA.PatternAny pos (Just name) type:
          Meta.USR module.umr name
      _:
          # TODO this algorithm should be ran on an intermediate representation, where all
          # root patterns assignments are actually expanded
          # maybe have a constructor accessor operation in the IR AST?
          todo "TODO makeUsr does not implement pattern defs"













###

alias Env =
    Dict Name [RankDeps]


blockDependencies as Env: [CA.Statement]: [RankDeps] =
    env: block:

    f & s =
        env & []
            >> List.for block statement: (envX & depsAccum):
                    statementDependencies envX statement
                          >> Tuple.mapSecond (merge depsAccum)
    s



statementDependencies as Env: CA.Statement: Env & [RankDeps] =
    env: statement:

    try statement as
        CA.Evaluation expr:
            env & expressionDependencies env expr

        CA.Definition def:
            deps =
                blockDependencies env def.body

            newEnv =
                env
                    >> Dict.for (CA.patternNames def.pattern) name: _:
                        # TODO use Dict.update and merge with whatever was there first
                        Dict.insert name deps

            newEnv & List.take 1 deps


expressionDependencies as Env: CA.Expression: [RankDeps] =
    env: expression:

    try expression as
        CA.Lambda pos param body:
            Dict.empty :: blockDependencies env body

        CA.Variable _ args:
            try args.ref as
                CA.RefRoot usr:
                    [ Dict.singleton usr 0 ]

                CA.RefBlock name:
                    try Dict.get name env as
                        Just varDeps:
                            varDeps

                        Nothing:
                            [ Dict.empty ]

        CA.LiteralNumber _ _:
            []

        CA.LiteralText _ _:
            []

        CA.Constructor _ _:
            []

        CA.Record _ maybeExtVarArgs attrs:
          x =
            try maybeExtVarArgs as
              Just { ref = CA.RefRoot usr }:
                  [Dict.singleton usr 0]
              _:
                  [Dict.empty]

          x >> Dict.for attrs name: value:
              merge (expressionDependencies env value)

        CA.Call _ ref arg:
            [#
                Consider the function call:

                    (someFn someArg)

                `expressionDependencies env ref` will give us `[ Dict.singleton (USR someFn) 0 ]`

                But because `someFn` will be called, then we need to raise its rank from 0 to 1!
            #]
            refDeps =
                ref
                    >> expressionDependencies env
                    >> List.map (Dict.map k: rank: rank + 1)

            merge refDeps (argumentDependencies env arg)

        CA.If _ { condition, true, false }:
            blockDependencies env condition
              >> merge (blockDependencies env true)
              >> merge (blockDependencies env false)

        CA.Try _ expression pasAndBlocks:
            expressionDependencies env expression
                >> List.for pasAndBlocks (pattern & block):
                    merge (blockDependencies env block)



argumentDependencies as Env: CA.Argument: [RankDeps] =
    env: arg:

    try arg as
      CA.ArgumentMutable _ _:
          # Mutation cannot reference root values and cannot contain functions, so no deps
          []

      CA.ArgumentExpression e:
          [ collapse (expressionDependencies env e) ]

