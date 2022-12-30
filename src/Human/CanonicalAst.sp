

andThen as (a: StateMonad.M state b): StateMonad.M state a: StateMonad.M state b =
    StateMonad.andThen


get as (state: a): StateMonad.M state a =
    StateMonad.get


return as a: StateMonad.M state a =
    StateMonad.return


parensIf as Bool: Text: Text =
   test: s:
    if test then
        "(" .. s .. ")"

    else
        s


#
# Ref
#

usrToText as UMR: Meta: USR: Text =
    currentUmr: meta: usr:

    USR moduleUmr name =
        usr

    if currentUmr == moduleUmr then
        name
    else
        maybeGlobal =
            meta.globalTypes
                # TODO Dict.find?
                >> Dict.toList
                >> List.find ((k & v): v == usr)

        try maybeGlobal as
            Just (k & v):
                k

            Nothing:
                try Dict.get moduleUmr meta.umrToModuleVisibleAs as
                    Just moduleAlias:
                        moduleAlias .. "." .. name

                    Nothing:
                        UMR souece modulePath =
                            moduleUmr

                        # INFORMATION LOSS: source
                        modulePath .. "." .. name


#
# Type
#
typeToText as UMR: Meta: CA.Type: Text =
    currentUmr: meta: t:
    t >> typeToPriorityAndText currentUmr meta >> Tuple.second


typeToPriorityAndText as UMR: Meta: CA.Type: Int & Text =
    currentUmr: meta: type:

    parensIfGreaterThan as Int: CA.Type: Text =
        threshold: ty:

        pri & str =
            typeToPriorityAndText currentUmr meta ty

        parensIf (pri > threshold) str

    CA.Type _ type_ =
        type

    try type_ as
        CA.TypeNamed usr uni args:
            ( (if args == [] then 0 else 1) & (usrToText currentUmr meta usr :: List.map (parensIfGreaterThan 0) args >> Text.join " "))

#        CA.TypeNamed pos usr args:
#            0 & usrToText currentUmr meta usr
#            typeToPriorityAndText currentUmr meta ty2
            # TODO: once we can display a type on multiple lines, we should expand the alias?
              [#

                 , [ "<"
                   , name
                   , "="
                   , typeToString ty2
                   , ">"
                   ]
                     >> Text.join " "
              #]

        CA.TypeAnnotationVariable uni name:
            ( 0 & name)

        CA.TypeFn pars to:
            todo "CA.TypeFn"
#            arrow =
#                try lambdaModifier as
#                    LambdaNormal: ": "
#                    LambdaConsuming: ":- "
#
#            ( 2 & ([ parensIfGreaterThan 1 from , arrow , parensIfGreaterThan 2 to ] >> Text.join ""))

        CA.TypeRecord uni attrs:

            attrsString =
                attrs
                >> Dict.toList
                >> List.sortBy Tuple.first
                >> List.map (( name & ty ): name .. " as " .. typeToText currentUmr meta ty)
                >> Text.join ", "

            l = [
              , "{"
              , attrsString
              , "}"
              ]

            ( 0 & Text.join " " l)

#        CA.TypeRecordExt pos name flags attrs:
#
#            attrsString =
#                attrs
#                    >> Dict.toList
#                    >> List.sortBy Tuple.first
#                    >> List.map (( name & ty ): name .. " as " .. typeToText currentUmr meta ty)
#                    >> Text.join ", "
#            l = [
#              , "{"
#              , name .. " with"
#              , attrsString
#              , "}"
#              ]
#
#            ( 0 & Text.join " " l)



#
# Type Variables
#
# Humanize generated tvar names, ie "2": "a"
#


alias NormMonad a =
    StateMonad.M NormState a


alias NormState =
    { replacements as Dict Text Text
    , next as Int
    }


initNstate as NormState =
    { replacements = Dict.empty
    , next = 0
    }


intToName as Int: a: Text =
    n: acc:

    try n as
        0: "a"
        1: "b"
        2: "c"
        3: "d"
        4: "e"
        5: "f"
        _: todo "intToName"

[#
    as Int: List Char: Text
    #
    # TODO this is wrong, because sometimes `a` is used as 0 sometimes as 1
    #
    # works: 26 * 26: "aaa"
    # doesnt: 26 * 26 - 1: "zz"
    #
    # Don't want to think about this right now
    #
    zero =
        0 #Char.toCode 'a'

    nine =
        9 #Char.toCode 'z'

    base =
        nine - zero + 1

    head =
        modBy base n + zero

    r =
        n / base

    if r == 0 then
        # `a` is zero, so the most significant symbol will start at `b` unless we remove 1
        # (per above, this is wrong)
        if acc == [] then
            Text.fromList [ Char.fromCode head ]

        else
            Text.fromList (Char.fromCode (head - 1) :: acc)

    else
        intToName r (Char.fromCode head :: acc)
#]


newName as NormMonad Text =
    state:
    ( intToName state.next [] & { state with next = state.next + 1 })


normalizeType as CA.Type: CA.Type =
    t:
    t >> normType >> StateMonad.run initNstate >> Tuple.first


normalizeTypeAndTyvars as CA.Type: Dict Text a: ( CA.Type & Dict Text a ) =
    tyOld: tyvarsOld:

    ( tyNew & state ) =
        tyOld
            >> normType
            >> StateMonad.run initNstate

    replace = name:
        Dict.get name state.replacements
            >> Maybe.withDefault name
            >> Dict.insert

    tyvarsNew =
        Dict.for tyvarsOld replace Dict.empty

    ( tyNew & tyvarsNew )


normType as CA.Type: NormMonad CA.Type =
    (CA.Type pos ty_):
    try ty_ as
        CA.TypeNamed uni name args:
            StateMonad.list_map normType args >> andThen args_n:
            return << CA.Type pos << CA.TypeNamed uni name args_n

#        CA.TypeAlias pos usr args:
#            StateMonad.list_map normType args >> andThen args_n:
#            return << CA.TypeAlias pos usr args_n

        CA.TypeAnnotationVariable uni name:
            normName name >> andThen n:
            return << CA.Type pos << CA.TypeAnnotationVariable uni n

        CA.TypeFn pars to0:
            todo "CA.TypeFn"
#            (normType from0) >> andThen from1:
#            (normType to0) >> andThen to1:
#            return << CA.TypeFn pos from1 fromIsMut to1

        CA.TypeRecord uni attrs0:
            (StateMonad.dict_map (k: normType) attrs0) >> andThen attrs1:
            return << CA.Type pos << CA.TypeRecord uni attrs1

#        CA.TypeRecordExt pos name0 flags attrs0:
#            normName name0 >> andThen name1:
#            StateMonad.dict_map (k: normType) attrs0 >> andThen attrs1:
#            return << CA.TypeRecordExt pos name1 flags attrs1



normName as Text: NormMonad Text =
    name:
    # Replace only names that are numbers ("22") or single letters ("a")
    if Text.length name > 1 and Text.toNumber name == Nothing then
        return name

    else
        get (x: x.replacements) >> andThen n2l:
        try Dict.get name n2l as
            Just replacement:
                return replacement

            Nothing:
                newName >> andThen n:

                addReplacement as NormState: NormState =
                    s:
                    { s with replacements = Dict.insert name n s.replacements }

                (StateMonad.update addReplacement) >> andThen _:
                return n
