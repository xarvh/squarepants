

allNatives =
    as Dict Meta.UniqueSymbolReference JA.Name

    corelib m n =
        as Text: Text: Meta.UniqueSymbolReference
        Meta.USR (Meta.UMR (Meta.SourceDir "corelib") m) n

    ioModule n =
        Meta.USR (Meta.UMR (Meta.SourceDir "posix") "IO") n

    Dict.empty
        >> Dict.for nativeBinops (fn symbol v: Dict.insert (Meta.spCoreUSR symbol) v.fnName)
        #
        >> Dict.insert CoreTypes.true "true"
        >> Dict.insert CoreTypes.false "false"
        >> Dict.insert CoreTypes.noneValue "null"
        #
        #>> Dict.insert "not" "sp_not"
        >> Dict.insert Prelude.debugLog.usr "sp_log"
        >> Dict.insert Prelude.debugTodo.usr "sp_todo"
        >> Dict.insert Prelude.debugToHuman.usr "sp_toHuman"
        >> Dict.insert Prelude.compare.usr "basics_compare"
        >> Dict.insert (corelib "Basics" "modBy") "basics_modBy"
        #
        >> Dict.insert (corelib "Text" "fromNumber") "text_fromNumber"
        >> Dict.insert (corelib "Text" "toNumber") "text_toNumber"
        >> Dict.insert (corelib "Text" "split") "text_split"
        >> Dict.insert (corelib "Text" "length") "text_length"
        >> Dict.insert (corelib "Text" "slice") "text_slice"
        >> Dict.insert (corelib "Text" "startsWith") "text_startsWith"
        >> Dict.insert (corelib "Text" "startsWithRegex") "text_startsWithRegex"
        >> Dict.insert (corelib "Text" "replaceRegex") "text_replaceRegex"
        >> Dict.insert (corelib "Text" "trimLeft") "text_trimLeft"
        >> Dict.insert (corelib "Text" "dropLeft") "text_dropLeft"
        >> Dict.insert (corelib "Text" "forEach") "text_forEach"
        #
        >> Dict.insert (corelib "List" "sortBy") "list_sortBy"
        #
        >> Dict.insert (Meta.spCoreUSR "/") "sp_divide"
        >> Dict.insert (Meta.spCoreUSR "::") "sp_cons"
        >> Dict.insert (Meta.spCoreUSR "==") "sp_equal"
        >> Dict.insert (Meta.spCoreUSR "/=") "sp_not_equal"
        # Platform natives
        >> Dict.insert (ioModule "parallel") "io_parallel"
        >> Dict.insert (ioModule "readDir") "io_readDir"
        >> Dict.insert (ioModule "readFile") "io_readFile"
        >> Dict.insert (ioModule "writeFile") "io_writeFile"
        >> Dict.insert (ioModule "writeStdout") "io_writeStdout"


nativeUnops =
    as Dict Text { jsSymb as JA.Name }
    Dict.empty
        >> Dict.insert Prelude.unaryPlus.symbol { jsSymb = "+" }
        >> Dict.insert Prelude.unaryMinus.symbol { jsSymb = "-" }
        #>> Dict.insert Prelude.not_.symbol { jsSymb = "!" }


nativeBinops =
    as Dict Text { jsSymb as JA.Name, mutates as Bool, fnName as Text }
    Dict.empty
        # fnName is used for when the op is used as a function such as `(+)`
        >> Dict.insert "+" { jsSymb = "+", mutates = False, fnName = "add" }
        >> Dict.insert "*" { jsSymb = "*", mutates = False, fnName = "mul" }
        >> Dict.insert "-" { jsSymb = "-", mutates = False, fnName = "sub" }
        >> Dict.insert ":=" { jsSymb = "=", mutates = True, fnName = "mutass" }
        >> Dict.insert "+=" { jsSymb = "+=", mutates = True, fnName = "mutadd" }
        >> Dict.insert ".." { jsSymb = "+", mutates = False, fnName = "strcon" }
        >> Dict.insert ">" { jsSymb = ">", mutates = False, fnName = "greaterThan" }
        >> Dict.insert "<" { jsSymb = "<", mutates = False, fnName = "lesserThan" }
        >> Dict.insert ">=" { jsSymb = ">=", mutates = False, fnName = "greaterOrEqual" }
        >> Dict.insert "<=" { jsSymb = "<=", mutates = False, fnName = "lesserOrEqual" }
        >> Dict.insert "or" { jsSymb = "||", mutates = False, fnName = "or" }
        >> Dict.insert "and" { jsSymb = "&&", mutates = False, fnName = "and" }


nativeBinopToFunction spName { jsSymb, mutates, fnName } acc =
    as Text: { jsSymb as JA.Name, mutates as Bool, fnName as Text }: List JA.Statement: List JA.Statement

    d =
        Pos.N

    usr =
        CoreTypes.makeUsr spName

    a =
     [ CA.Evaluation
        (CA.Lambda d
            (CA.ParameterPattern << CA.PatternAny d (Just "a") Nothing)
            [ CA.Evaluation
                (CA.Lambda d
                    (CA.ParameterPattern << CA.PatternAny d (Just "b") Nothing)
                    [ CA.Evaluation
                        (CA.Call d
                            (CA.Call d
                                (CA.Variable d { attrPath = [], ref = CA.RefRoot usr })
                                (CA.ArgumentExpression
                                    (CA.Variable d { attrPath = [], ref = CA.RefBlock "a" })
                                )
                            )
                            ((if mutates: CA.ArgumentMutable d else fn x: x >> CA.Variable d >> CA.ArgumentExpression)
                                { attrPath = [], ref = CA.RefBlock "b" }
                            )
                        )
                    ]
                )
            ]
        )
     ]
        >> translateBodyToExpr { mutables = Set.empty, errorEnv = { moduleByName = Dict.empty } }
        >> JA.Define fnName

    a :: acc


nativeBinopsAsFns =
    Dict.foldl nativeBinopToFunction nativeBinops []



[#
# Mutation
#

  CA.Definition: `x @= 0`

     => `const x = { obj : { m : 0 }, attr : 'm' };`



  CA.Variable: `x`

     => `$clone$(x.obj[x.attr])`

     (when we have a non-variable type we can optimize this)



  CA.Call (mutation primitive): `@x += 1`

      CA.ArgumentMutable, fn is primitive: `@x := 0`

        => `x.obj[x.attr] := 0`


      CA.ArgumentMutable, fn is NOT primitive
        if attrPath == []: `doStuff @x`

          => `doStuff(x)`


        else: `doStuff @x.a.b.c`

          => `doStuff({ obj: x.obj[x.attr].a.b, attr: 'c' })



#]


alias Env =
    { mutables as Set JA.Name
    , errorEnv as Error.Env
    }


wrapMutable mutable expr =
    as Bool: JA.Expr: JA.Expr
    if mutable:
        Dict.empty
            >> Dict.insert "attr" (JA.Literal << quoteAndEscape "$")
            >> Dict.insert "obj" (JA.Record << Dict.singleton "$" expr)
            >> JA.Record

    else
        expr


unwrapMutable x =
    as JA.Name: JA.Expr
    ja_x =
        JA.Var x
    # $x.obj[$x.attr]
    JA.AccessWithBrackets (JA.AccessWithDot "attr" ja_x) (JA.AccessWithDot "obj" ja_x)


clone expr =
    as JA.Expr: JA.Expr
    JA.Call (JA.Var "sp_clone") [ expr ]



[#
 # Name translations
 #


   We need to give JS names to
     - the variables declared within the SP code
     - the variables we generate as part of the SP->JS translation

   and we need to ensure that these names do not clash with each other
   and do not clash with JS reserved keywords.

   So when we need a new naming schema, we follow this:

   * no $ anywhere
     => it's a native JS value

   * starts and ends with a single $
     => $clone$

   * starts with a single $
     * followed by a letter
       => it corresponds to an SP path or local value
     * followed by a number
       => it's a constructor argument

     * starts with double $$
       => $$try
       => main name of an unpacked pattern

#]


translateRef ref =
    as CA.Ref: JA.Name

    try ref as
        CA.RefBlock name:
            "$" .. name

        CA.RefRoot usr:
            translateUsr usr


translateUsr usr =
    as Meta.UniqueSymbolReference: Text

    try Dict.get usr allNatives as
        Just nv:
            nv

        Nothing:
            Meta.USR (Meta.UMR source modulePath) name =
                usr

            "$" .. translateSource source .. "$" .. Text.replace "/" "$" modulePath .. "$" .. name


translateSource src =
    as Meta.Source: Text

    # TODO just generate an id for each source, to avoid all stupid collisions and invalid characters
    try src as
        Meta.Core:
            "core"

        Meta.SourceDir path:
            if Text.startsWithRegex "[a-zA-Z0-9_./]*$" path == "":
                Debug.todo << "Invalid chars in source dir name: " .. path
            else
                Text.replace "." "_" path


constructorArgumentName i =
    as Int: JA.Name
    "$" .. Text.fromNumber i


tryName =
    as JA.Name
    "$$try"


pickMainName pattern =
    as CA.Pattern: Maybe JA.Name

    x =
        try pattern as
            CA.PatternAny _ (Just name) _:
                Just << "$" .. name

            _:
                pattern
                    >> CA.patternNames
                    >> Dict.keys
                    >> List.head
                    >> Maybe.map (fn s: s .. "$$")

    Maybe.map (fn s: s >> Text.replace "/" "$" >> Text.replace "." "$") x



#
# Translation
#


getValueDefName def =
    as CA.ValueDef: Text
    def.pattern
        >> CA.patternNames
        >> Dict.keys
        >> List.head
        >> Maybe.withDefault "BLARGH"




defIsFunction def =
    as CA.ValueDef: Bool
    try def.body as
        CA.Evaluation (CA.Lambda _ _ _) :: _ :
            True
        _:
            False


makeUsr module name =
    as CA.Module: Name: Meta.UniqueSymbolReference
    Meta.USR module.umr name


# TODO rename to RootDef or something?
union Node =
    , NodeDef Meta.UniqueSymbolReference CA.ValueDef
    , NodeName Meta.UniqueSymbolReference JA.Statement


#isMeta (Meta.USR (Meta.UMR _ n) _) =
#    log "N" n
#    n == "Types/Meta"



reorderModuleValues modules =
    as [CA.Module]: Result [Meta.UniqueSymbolReference] [Node]

    log "nodesByUsr" ""

    # Assign an USR to each name of each ValueDef
    nodesByUsr =
        as Dict Meta.UniqueSymbolReference Node
        Dict.empty >>
            List.for modules fn module:
                Dict.for module.valueDefs fn _ def accum:
                    try def.pattern as
                        CA.PatternAny _ (Just name) _:
                            usr = makeUsr module name
                            if Dict.member usr allNatives:
                                accum
                            else
                                Dict.insert usr (NodeDef usr def) accum

                        _:
                            try pickMainName def.pattern as
                                Nothing:
                                    accum

                                Just mainName:
                                    mainUsr =
                                        makeUsr module mainName

                                    jsStatementsByUsr =
                                        as Dict Meta.UniqueSymbolReference JA.Statement
                                        assignPattern
                                            (fn n: CA.RefRoot << makeUsr module n)
                                            (fn n stat: Dict.insert (makeUsr module n) stat)
                                            def.pattern
                                            (JA.Var mainName)
                                            Dict.empty

                                    accum
                                        >> Dict.insert mainUsr (NodeDef mainUsr def)
                                        >> Dict.for jsStatementsByUsr fn usr stat a:
                                            Dict.insert usr (NodeName mainUsr stat) a


    log "nonFunctionsByUsr" ""

    nonFunctionsByUsr & functions =
        as Dict Meta.UniqueSymbolReference Node & [Node]
        Dict.empty & [] >>
            Dict.for nodesByUsr fn usr node (nf & f):
                try node as
                    NodeName _ _:
                        Dict.insert usr node nf & f

                    NodeDef _ def:
                        if defIsFunction def:
                            nf & (node :: f)
                        else
                            Dict.insert usr node nf & f

#    fullDeps @=
#        as Dict Meta.UniqueSymbolReference (Set Meta.UniqueSymbolReference)
#        Dict.empty

    getFullDependencies ongoing usr node =
        as Set Meta.UniqueSymbolReference: Meta.UniqueSymbolReference: Node: Set Meta.UniqueSymbolReference

        try node as
            NodeName d_usr _:
                Set.singleton d_usr

            NodeDef _ def:
#                try Dict.get usr fullDeps as
#                    Just deps:
#                        deps
#
#                    Nothing:
                        newOngoing =
                            Set.insert usr ongoing

                        deps =
                            def.directValueDeps
                                >> Dict.for def.directValueDeps fn d_usr _:
                                    if Set.member d_usr newOngoing:
                                        identity
                                    else
                                        try Dict.get d_usr nodesByUsr as
                                            Nothing:
                                                # native ops are not in nodesByUsr, skip them
                                                identity
                                            Just d_node:
                                                Set.join (getFullDependencies newOngoing d_usr d_node)

#                        @fullDeps := Dict.insert usr deps fullDeps

                        deps



    log "nonFunctionsFullDependenciesByUsr" ""

    nonFunctionsFullDependenciesByUsr =
        as Dict Meta.UniqueSymbolReference (Set Meta.UniqueSymbolReference)
        nonFunctionsByUsr >> Dict.map fn usr node:
            Dict.intersect (getFullDependencies Set.empty usr node) nonFunctionsByUsr


#    List.each (Dict.toList nonFunctionsFullDependenciesByUsr) fn (usr & deps):
#        if isMeta usr:
#            log ">" (usr & Dict.keys deps)
#            None
#        else
#            None


    log "reorder" ""

    nodeToDeps node =
        as Node: Set Meta.UniqueSymbolReference
        try node as
            NodeName d_usr _:
                Set.singleton d_usr

            NodeDef usr def:
                try Dict.get usr nonFunctionsFullDependenciesByUsr as
                    Just deps:
                        deps
                    Nothing:
                        Debug.todo "nodeToDeps this should not happen?"

    RefHierarchy.reorder nodeToDeps nonFunctionsByUsr >> Result.map fn reorderedNonFunctionNodes:

#        List.each reorderedNonFunctionNodes fn n:
#            try n as
#                NodeName usr _: log "*" usr
#                NodeDef usr _: log "*" usr

        List.concat [ functions, reorderedNonFunctionNodes ]



translateAll eenv globals modules =
    as Error.Env: CA.Globals: [CA.Module]: List JA.Statement

    try reorderModuleValues modules as
        Err circular:
            log "translateAll: circular" ""
            List.each circular fn usr:
                log "^" usr
            Debug.todo "translateAll circular"

        Ok nodes:
            env =
                { mutables = Set.empty
                , errorEnv = eenv
                }

            values =
                as [JA.Statement]
                nodes
                    >> List.map (translateNode env)

            constructors =
                as [JA.Statement]
                [] >> Dict.for globals.constructors fn usr cons acc:
                    if Dict.member usr allNatives:
                        acc
                    else
                        translateUnionConstructor usr cons :: acc

            List.concat [ nativeBinopsAsFns, constructors, values ]




translateNode env node =
    as Env: Node: JA.Statement

    try node as
        NodeName usr jaStatement:
            jaStatement

        NodeDef usr valueDef:
            valueDef.body
                >> translateBodyToExpr env
                >> JA.Define (translateUsr usr)


translateLocalValueDef env caDef =
    as Env: CA.ValueDef: ( List JA.Statement & Env )
    if caDef.body == []:
        ( [] & env )

    else
        try pickMainName caDef.pattern as
            Nothing:
                ( [ JA.Eval (translateBodyToExpr env caDef.body) ]
                & env
                )

            Just mainName:

                a =
                    caDef.body
                        >> translateBodyToExpr env
                        >> wrapMutable caDef.mutable
                        >> JA.Define mainName
                b =
                  if caDef.mutable:
                    { env with mutables = Set.insert mainName env.mutables }

                  else
                    env

                ( a :: patternDefinitions mainName caDef.pattern ) & b


translateStatement env s =
    as Env: CA.Statement: ( List JA.Statement & Env )
    try s as
        CA.Definition def:
            translateLocalValueDef env def

        CA.Evaluation expr:
            ( [ JA.Eval (translateExpr env expr) ]
            & env
            )


translateVar env { ref, attrPath } =
    as Env: CA.VariableArgs: JA.Expr

    jname =
        translateRef ref

    if Set.member jname env.mutables:
        jname
            >> unwrapMutable
            >> accessAttrs attrPath
            >> clone

    else
        accessAttrs attrPath (JA.Var jname)


translateExpr env expression =
    as Env: CA.Expression: JA.Expr
    try expression as
        CA.LiteralNumber _ num:
            JA.Literal << Text.fromNumber num

        CA.LiteralText _ text:
            JA.Literal << quoteAndEscape text

        CA.Variable _ ar:
            translateVar env ar

        CA.Constructor _ usr:
            try Dict.get usr allNatives as
                Nothing:
                    JA.Var (translateUsr usr)
                Just n:
                    JA.Var n

        CA.Lambda pos parameter body:
            ( args & extraJaStatements & localEnv ) =
                try parameter as
                    CA.ParameterMutable _ name:
                        jaName =
                            "$" .. name
                        ( [ jaName ]
                        & []
                        & { env with mutables = Set.insert jaName env.mutables }
                        )

                    CA.ParameterPattern pattern:
                        try pickMainName pattern as
                            Nothing:
                                ( []
                                & []
                                & env
                                )

                            Just mainName:
                                ( [ mainName ]
                                & patternDefinitions mainName pattern
                                & env
                                )

            try translateBodyToEither localEnv extraJaStatements body as
                Left expr:
                    JA.SimpleLambda args expr

                Right block:
                    JA.BlockLambda args block

        CA.Record _ extends attrs:
            obj =
                attrs
                    >> Dict.map (fn k: translateExpr env)
                    >> JA.Record
            try extends as
                Nothing:
                    obj

                Just extend:
                    JA.Call (JA.Var "Object.assign")
                        [ JA.Record Dict.empty
                        , translateVar env extend
                        , obj
                        ]

        CA.Call _ ref arg:
            try maybeNativeBinop env ref arg as
                Just jaExpr:
                    jaExpr

                Nothing:
                    jsArg =
                        translateArg { nativeBinop = False } env arg
                    try maybeNativeUnop env ref jsArg as
                        Just jaExpr:
                            jaExpr

                        Nothing:
                            JA.Call (translateExpr env ref) [ jsArg ]

        CA.If _ ar:
            JA.Conditional
                (translateBodyToExpr env ar.condition)
                (translateBodyToExpr env ar.true)
                (translateBodyToExpr env ar.false)

        CA.Try pos value tries:
            [#

               (() => {
                 const $$try = #value;

                 if (#pattern1Conditions) {
                   #pattern1Assignment
                   #pattern1Block
                   return
                 }

                 if (#pattern2Conditions) {
                   #pattern2Assignment
                   #pattern2Block
                   return
                 }
               )


            #]
            head =
                JA.Define tryName (translateExpr env value)

            init =
                JA.Var tryName

            testPa ( pattern & block ) =
                extraStats =
                    assignPattern CA.RefBlock (fn _ stat acc: stat :: acc) pattern (JA.Var tryName) []

                condition =
                    testPattern pattern init []
                        >> binopChain (JA.Literal "true") "&&"

                whenConditionMatches =
                    try translateBodyToEither env extraStats block as
                        Left e:
                            List.concat [extraStats, [ JA.Return e ] ]

                        Right bl:
                            # TODO two returns is better than zero, but one would be better than two...
                            List.concat [ bl, [ JA.Return (JA.Literal "null") ]]

                JA.If condition whenConditionMatches

            human =
                Error.posToHuman env.errorEnv pos

            default =
                [ JA.Literal "'Missing pattern in try..as'"
                , JA.Literal ("'" .. human.location .. "'")
                , JA.Call (JA.Literal "sp_toHuman") [ JA.Var tryName ]
                # JA.Call (JA.Literal "console.log") [ JA.Call (JA.Literal "JSON.stringify") [ JA.Var tryName ]]
                ]
                    >> JA.Call (JA.Literal "sp_throw")
                    >> JA.Eval

            allStatements =
                List.concat [ (head :: List.map testPa tries), [ default ] ]

            JA.Call (JA.BlockLambda [] allStatements) []


translateArg { nativeBinop } env arg =
    as { nativeBinop as Bool }: Env: CA.Argument: JA.Expr
    try arg as
        CA.ArgumentExpression e:
            translateExpr env e

        CA.ArgumentMutable _ { ref, attrPath }:
            if nativeBinop:
                #SP: @x.a.b += blah
                #JS: x.obj[x.attr].a.b += blah
                ref
                    >> translateRef
                    >> unwrapMutable
                    >> accessAttrs attrPath

            else
                try attrPath as
                    []:
                        #SP: doStuff @x
                        #JS: doStuff(x)
                        ref
                            >> translateRef
                            >> JA.Var

                    head :: tail:
                        #SP: doStuff @x.a.b.c
                        #JS: doStuff({ obj: x.obj[x.attr].a.b, attr: 'c' })
                        ref
                            >> translateRef
                            >> unwrapMutable
                            >> accessAttrsButTheLast head tail
                            >>
                               (fn ( wrappedExpr & lastAttrName ):
                                    Dict.empty
                                        >> Dict.insert "obj" wrappedExpr
                                        >> Dict.insert "attr" (JA.Literal << quoteAndEscape lastAttrName)
                                        >> JA.Record
                               )


accessAttrs attrPath e =
    as List Text: JA.Expr: JA.Expr
    List.foldl JA.AccessWithDot attrPath e


accessAttrsButTheLast attrHead attrTail e =
    as Text: List Text: JA.Expr: ( JA.Expr & Text )
    fold attr ( expr & last ) =
        ( JA.AccessWithDot last expr
        & attr
        )
    List.foldl fold attrTail ( e & attrHead )


maybeNativeUnop env reference argument =
    as Env: CA.Expression: JA.Expr: Maybe JA.Expr
    try reference as
        CA.Variable _ { ref = CA.RefRoot (Meta.USR _ name) }:
            try Dict.get name nativeUnops as
                Nothing:
                    Nothing

                Just { jsSymb }:
                    JA.Unop jsSymb argument >> Just

        _:
            Nothing


none =
    as Text
    translateUsr CoreTypes.noneValue


maybeNativeBinop env reference argument =
    as Env: CA.Expression: CA.Argument: Maybe JA.Expr
    try reference as
        # TODO check that USR sorce is Core?
        CA.Call _ (CA.Variable _ { ref = CA.RefRoot (Meta.USR _ name) }) rightArg:
            try Dict.get name nativeBinops as
                Nothing:
                    Nothing

                Just { jsSymb, mutates }:
                    cons =
                        if mutates:
                            JA.Mutop jsSymb none

                        else
                            JA.Binop jsSymb
                    z =
                        cons
                            (translateArg { nativeBinop = True } env argument)
                            (translateArg { nativeBinop = True } env rightArg)

                    Just z

        _:
            Nothing


binopChain default op es =
    as JA.Expr: Text: List JA.Expr: JA.Expr
    try es as
        []:
            default

        head :: tail:
            List.foldl (JA.Binop op) tail head


union Either a b =
    , Left a
    , Right b


translateBodyToExpr env caBody =
    as Env: List CA.Statement: JA.Expr
    try translateBodyToEither env [] caBody as
        Left e:
            e

        Right block:
            JA.Call (JA.BlockLambda [] block) []


translateBodyToEither env extra caBody =
    as Env: List JA.Statement: List CA.Statement: Either JA.Expr (List JA.Statement)

    reversedStats & env1 =
        extra & env
            >> List.for caBody fn caStat (statsAccum & envX0):

                newStats & envX1 =
                    translateStatement envX0 caStat

                allStats =
                    statsAccum
                        >> List.for newStats fn stat acc: stat :: acc

                allStats & envX1

    try reversedStats as
        []:
            JA.Var "null"
                >> Left

        [ JA.Eval single ]:
            single
                >> Left

        oldLast :: rest:
            (try oldLast as
                JA.Eval expr:
                    JA.Return expr :: rest

                _:
                    JA.Return (JA.Var none) :: oldLast :: rest
            )
                >> List.reverse
                >> Right



#
#
#


quoteAndEscape s =
    as Text: Text
    escaped =
        s
            >> Text.replace "\n" "\\n"
            >> Text.replace "\"" "\\\""
    "\"" .. escaped .. "\""


accessWithBracketsInt index =
    as Int: JA.Expr: JA.Expr
    index
        >> Text.fromNumber
        >> JA.Var
        >> JA.AccessWithBrackets



#
# Pattern break down
#


testPattern pattern valueToTest accum =
    as CA.Pattern: JA.Expr: List JA.Expr: List JA.Expr
    try pattern as
        CA.PatternAny _ _ _:
            accum

        CA.PatternLiteralText _ text:
            JA.Binop "===" (JA.Literal << quoteAndEscape text) valueToTest :: accum

        CA.PatternLiteralNumber _  num:
            JA.Binop "===" (JA.Literal << Text.fromNumber num) valueToTest :: accum

        CA.PatternConstructor _ usr pas:
            head =
                # this is necessary because we use true, false and null for True, False and None respectively
                try Dict.get usr allNatives as
                    Just nv:
                        JA.Binop "===" (JA.Literal nv) (valueToTest)

                    Nothing:
                        #Meta.USR umr name = usr
                        JA.Binop "===" (JA.Literal (quoteAndEscape << translateUsr usr)) (accessWithBracketsInt 0 valueToTest)

            foldArg argPattern ( index & acc ) =
                ( index + 1
                & testPattern argPattern (accessWithBracketsInt index valueToTest) acc
                )

            ( 1 & (head :: accum) )
                >> List.foldl foldArg pas
                >> Tuple.second

        CA.PatternRecord _ attrs:
            foldAttr name pa =
                testPattern pa (JA.AccessWithDot name valueToTest)
            Dict.foldl foldAttr attrs accum



assignPattern nameToReference insert pattern exprAccum accum =
    as (Name: CA.Ref): (Name: JA.Statement: a: a): CA.Pattern: JA.Expr: a: a
    try pattern as
        CA.PatternAny _ Nothing _:
            accum

        CA.PatternAny _ (Just name) _:
            insert name (JA.Define (translateRef << nameToReference name) exprAccum) accum

        CA.PatternLiteralNumber _ _:
            accum

        CA.PatternLiteralText _ _:
            accum

        CA.PatternConstructor _ path pas:
            foldEveryArgument ( index & pa ) =
                assignPattern nameToReference insert pa (accessConstructorArg (index + 1) exprAccum)

            List.foldl foldEveryArgument (List.indexedMap Tuple.pair pas) accum

        CA.PatternRecord _ attrs:
            accum
                >> Dict.for attrs fn name pa:
                    assignPattern nameToReference insert pa (JA.AccessWithDot name exprAccum)


patternDefinitions mainName pattern =
    as JA.Name: CA.Pattern: List JA.Statement
    try pattern as
        CA.PatternAny pos (Just _) _:
            []

        _:
            assignPattern CA.RefBlock (fn _ s a: s :: a) pattern (JA.Var mainName) []



#
# Union Constructors
#


accessConstructorArg =
    as Int: JA.Expr: JA.Expr
    accessWithBracketsInt


translateUnionConstructor usr { type, args } =
    as Meta.UniqueSymbolReference: CA.Constructor: JA.Statement
    # const ConstructorName = ($1) => ($2) => [ "ConstructorName", $1, $2 ]
    n =
        List.length args

    range =
        List.range 1 n

    storedArgs =
        List.map (fn x: x >> constructorArgumentName >> JA.Var) range

    name =
        quoteAndEscape << translateUsr usr

    expr =
        JA.Array (JA.Literal name :: storedArgs)

    lambdas =
        List.foldr (fn i: JA.SimpleLambda [ constructorArgumentName i ]) range expr

    JA.Define (translateUsr usr) lambdas


not_ bool =
    as Bool: Bool
    if bool: False else True


listCons =
    as Text
    translateUsr CoreTypes.cons


listNil =
    as Text
    translateUsr CoreTypes.nil


# This messes with Vim's syntax highlight, so I'm moving it at the end
nativeDefinitions =
    as Text
    """#!/usr/bin/env node

const sp_clone = (src) => {
 if (Array.isArray(src))
   return src.map(sp_clone);

 if (typeof(src) === 'object') {
   const dest = {};
   for (let k in src) { dest[k] = sp_clone(src[k]); }
   return dest;
 }

 return src;
}


//
// Basic ops
//


const sp_equal = (a) => (b) => {
  if (a === b)
    return true

  if (Array.isArray(a)) {
    if (!Array.isArray(b)) return false;

    const l = a.length;
    if (l !== b.length) return false;

    let i = 0;
    while (i < l) {
      if (!sp_equal(a[i])(b[i])) return false;
      ++i;
    }

    return true;
  }

  if (typeof(a) === 'object') {
    if (typeof(b) !== 'object') return false;

    const keys = Object.keys(a);
    const l = keys.length;
    if (l !== Object.keys(b).length) return false;

    let i = 0;
    while (i < l) {
      let k = keys[i];
      if (!sp_equal(a[k])(b[k])) return false;
      ++i;
    }

    return true;
  }

  return false;
}


const sp_not_equal = (a) => (b) => {
  return !sp_equal(a)(b);
}


const sp_compare = (a, b) => {

  // union type
  if (Array.isArray(a)) {
    // compare constructor names
    if (a[0] > b[0]) return 1;
    if (b[0] > a[0]) return -1;
    for (let i = 1; i < a.length; i++) {
        const cmp = sp_compare(a[i], b[i]);
        if (cmp) return cmp;
    }
    return 0;
  }

  if (typeof a === 'object') {
    const keys = Object.keys(a).sort();
    for (let k of keys) {
        const cmp = sp_compare(a[k], b[k]);
        if (cmp) return cmp;
    }
    return 0;
  }

  if (a > b) return 1;
  if (a < b) return -1;
  return 0;
}

const sp_divide = (right) => (left) => {
  if (right === 0) return 0;
  return left / right;
}


const basics_modBy = (a) => (b) => b % a;

const basics_compare = (a) => (b) => sp_compare(a, b);


//
// Debug
//


const sp_todo = (message) => {
  throw new Error("TODO: " + message);
}


const sp_log = (message) => (thing) => {
  console.log(message, sp_toHuman(thing));
  return thing;
}


const sp_throw = function (errorName) {
    console.error(...arguments);
    throw new Error(errorName);
}


//
// To Human
//


const sp_toHuman = (a) => {

  if (Array.isArray(a))
    return sp_toHumanAsList([], a) || sp_toHumanAsUnion(a);

  if (typeof a === 'function') {
    return '<function>';
  }

  if (typeof a === 'object') {
    let x = [];
    for (let i in a) x.push(i + ' = ' + sp_toHuman(a[i]));
    return '{' + x.join(', ') + '}';
  }

  return JSON.stringify(a, null, 0);
}


const sp_toHumanAsUnion = (a) => {
  return a[0] + ' ' + a.slice(1).map(arg => '(' + sp_toHuman(arg) + ')').join(' ');
}


const sp_toHumanAsList = (arrayAccum, list) => {
  if (list[0] === '""" .. listCons .. """') {
    arrayAccum.push(sp_toHuman(list[1]));
    return sp_toHumanAsList(arrayAccum, list[2]);
  }

  if (list[0] === '""" .. listNil .. """')
    return '[' + arrayAccum.join(', ') + ']';

  return false;
}


//
// Text
//


const text_fromNumber = (n) => '' + n;

const text_toNumber = (t) => {
    const n = +t;

    // TODO this is super brittle, replace it with a reference to the actual constructor names.
    // Also, we need only the names, there is no point in specifying the whole path because they just need to be unique within the union type.
    // The type checker already ensures that they will never be compared to anything else.
    return isNaN(n) ? [ "$corelib$Maybe$Nothing" ] : [ "$corelib$Maybe$Just", n ];
}

const text_split = (separator) => (target) => array_toList(target.split(separator));

const text_length = (s) => s.length;

const text_slice = (start) => (end) => (s) => s.slice(start, end);

const text_startsWith = (sub) => (s) => s.startsWith(sub);

const text_startsWithRegex = (regex) => {
  let re;
  try {
    re = new RegExp('^' + regex);
  } catch (e) {
    return () => ""
  }

  return (s) => {
    let m = s.match(re);
    return m ? m[0] : "";
  }
}

const text_replaceRegex = (regex) => {
  let re;
  try {
    re = new RegExp(regex, 'g');
  } catch (e) {
    return () => () => ""
  }

  return (replacer) => (s) => s.replace(re, replacer);
}

const text_trimLeft = (s) => {
  return s.trimLeft();
}

const text_dropLeft = (n) => (s) => {
  return s.slice(n);
}

const text_forEach = (s) => (f) => {
  for (let i of s) f(i);
  return null;
}




//
// Lists and Arrays
//


const sp_cons = (list) => (item) => {
  return [ '""" .. listCons .. """', item, list];
}


const array_toList = (array) => {
  let length = array.length;
  let list = [ '""" .. listNil .. """' ];
  for (let i = length - 1; i >= 0; i--) {
      list = [ '""" .. listCons .. """', array[i], list ];
  }
  return list;
}


const list_toArray = (list) => {
  const array = [];
  const rec = (ls) => {
    if (ls[0] === '""" .. listNil .. """')
      return array;

    array.push(ls[1]);
    return rec(ls[2]);
  };

  return rec(list);
}


const list_sortBy = (f) => (list) => array_toList(list_toArray(list).sort((a, b) => sp_compare(f(a), f(b))));


//
// Platform: IO
//
const fs = require('fs');

const io_wrap = (f) => [ "IO.IO", f ];

const io_parallel = (iosAsList) => io_wrap((never) => {
    // as [IO a]: IO [a]

    const ios = list_toArray(iosAsList);

    // TODO actually run them in parallel!

    let arr = [];
    for (let io of ios) {
        const r = io[1](never);
        if (r[0] === "SPCore/Result.Ok")
            arr.push(r[1]);
        else
            return $corelib$Result$Err(r[1]);
    }

    return $corelib$Result$Ok(array_toList(arr));
});


const io_readDir = (dirPath) => io_wrap((never) => {
    // as Text: IO [Bool & Text]

    var entries;
    try {
        entries = fs.readdirSync(dirPath, { withFileTypes: true });
    } catch (e) {
        return $corelib$Result$Err(e.message);
    }

    return $corelib$Result$Ok(array_toList(entries.map((dirent) => ({
        first: dirent.isDirectory(),
        second: dirent.name,
    }))));
});


const io_readFile = (path) => io_wrap((never) => {
    // as Text: IO Text

    var content;
    try {
        content = fs.readFileSync(path, 'utf8');
    } catch (e) {
        return $corelib$Result$Err(e.message);
    }

    return $corelib$Result$Ok(content);
});


const io_writeFile = (path) => (content) => io_wrap((never) => {
    // as Text: Text: IO None

    try {
        fs.writeFileSync(path, content);
    } catch (e) {
        return $corelib$Result$Err(e.message);
    }

    return $corelib$Result$Ok(null);
});


const io_writeStdout = (content) => io_wrap((never) => {
    // as Text: IO None

    console.info(content);
    return $corelib$Result$Ok(null);
});

    """
