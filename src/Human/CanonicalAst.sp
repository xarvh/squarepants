

andThen as (a: StateMonad.M state b): StateMonad.M state a: StateMonad.M state b =
    StateMonad.andThen


get as (state: a): StateMonad.M state a =
    StateMonad.get


return as a: StateMonad.M state a =
    StateMonad.return


alias Type =
    CA.CanonicalType


parensIf as Bool: Text: Text =
   test: s:
    if test then
        "(" .. s .. ")"

    else
        s


#
# Ref
#

usrToText as Meta.UniqueModuleReference: Meta: Meta.UniqueSymbolReference: Text =
    currentUmr: meta: usr:

    Meta.USR moduleUmr name =
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
                        Meta.UMR souece modulePath =
                            moduleUmr

                        # INFORMATION LOSS: source
                        modulePath .. "." .. name


#
# Type
#
typeToText as Meta.UniqueModuleReference: Meta: Type: Text =
    currentUmr: meta: t:
    t >> typeToPriorityAndText currentUmr meta >> Tuple.second


typeToPriorityAndText as Meta.UniqueModuleReference: Meta: CA.CanonicalType: Int & Text =
    currentUmr: meta: type:

    parensIfGreaterThan as Int: Type: Text =
        threshold: ty:

        pri & str =
            typeToPriorityAndText currentUmr meta ty

        parensIf (pri > threshold) str

    try type as
        CA.TypeConstant pos usr args:
            ( (if args == [] then 0 else 1) & (usrToText currentUmr meta usr :: List.map (parensIfGreaterThan 0) args >> Text.join " "))

        CA.TypeMutable pos ty:
            0 & "@" .. typeToText currentUmr meta ty

        CA.TypeVariable pos name flags:
            ( 0 & name)

        CA.TypeFunction pos from lambdaModifier to:
            arrow =
                try lambdaModifier as
                    LambdaNormal: ": "
                    LambdaConsuming: ":- "

            ( 2 & ([ parensIfGreaterThan 1 from , arrow , parensIfGreaterThan 2 to ] >> Text.join ""))

        CA.TypeRecord pos attrs:

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

        CA.TypeRecordExt pos name flags attrs:

            attrsString =
                attrs
                    >> Dict.toList
                    >> List.sortBy Tuple.first
                    >> List.map (( name & ty ): name .. " as " .. typeToText currentUmr meta ty)
                    >> Text.join ", "
            l = [
              , "{"
              , name .. " with"
              , attrsString
              , "}"
              ]

            ( 0 & Text.join " " l)

        CA.TypeAlias pos usr ty2:
            ( 0 & usrToText currentUmr meta usr )
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


normalizeType as Type: Type =
    t:
    t >> normType >> StateMonad.run initNstate >> Tuple.first


normalizeTypeAndTyvars as Type: Dict Text a: ( Type & Dict Text a ) =
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


normType as Type: NormMonad Type =
    ty:
    try ty as
        CA.TypeConstant pos name args:
            (StateMonad.list_map normType args) >> andThen args_n:
            return << CA.TypeConstant pos name args_n

        CA.TypeVariable pos name flags:
            (normName name) >> andThen n:
            return << CA.TypeVariable pos n flags

        CA.TypeFunction pos from0 fromIsMut to0:
            (normType from0) >> andThen from1:
            (normType to0) >> andThen to1:
            return << CA.TypeFunction pos from1 fromIsMut to1

        CA.TypeRecord pos attrs0:
            (StateMonad.dict_map (k: normType) attrs0) >> andThen attrs1:
            return << CA.TypeRecord pos attrs1

        CA.TypeRecordExt pos name0 flags attrs0:
            normName name0 >> andThen name1:
            StateMonad.dict_map (k: normType) attrs0 >> andThen attrs1:
            return << CA.TypeRecordExt pos name1 flags attrs1

        CA.TypeMutable pos t:
            (normType t) >> andThen t1:
            return << CA.TypeMutable pos t1

        CA.TypeAlias pos path t:
            (normType t) >> andThen t1:
            return << CA.TypeAlias pos path t1


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
