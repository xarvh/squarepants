
then = StateMonad.then
get = StateMonad.get
return = StateMonad.return

alias Type = CA.Type


parensIf test s =
    as Bool: Text: Text
    if test:
        "(" .. s .. ")"

    else
        s


#
# Ref
#

typeRefToText currentUmr meta ref =
    as Meta.UniqueModuleReference: Meta: CA.Ref: Text

    try ref as
        CA.RefBlock name:
            name

        CA.RefRoot usr:

            Meta.USR moduleUmr name =
                usr

            if currentUmr == moduleUmr:
                name
            else
                maybeGlobal =
                    meta.globalTypes
                        # TODO Dict.find?
                        >> Dict.toList
                        >> List.find (fn (k & v): v == usr)

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
typeToText currentUmr meta t =
    as Meta.UniqueModuleReference: Meta: Type: Text
    t >> typeToPriorityAndText currentUmr meta >> Tuple.second


typeToPriorityAndText currentUmr meta type =
    as Meta.UniqueModuleReference: Meta: CA.Type: Int & Text

    parensIfGreaterThan threshold ty =
        as Int: Type: Text

        pri & str =
            typeToPriorityAndText currentUmr meta ty

        parensIf (pri > threshold) str

    try type as
        CA.TypeConstant pos ref args:
            ( (if args == []: 0 else 1)
            & (typeRefToText currentUmr meta ref :: List.map (parensIfGreaterThan 0) args >> Text.join " ")
            )

        CA.TypeVariable pos name:
            ( 0 & name)

        CA.TypeFunction pos from fromIsMut to:
            arrow =
                if fromIsMut:
                    " @: "

                else
                    ": "
            ( 2 & ([ parensIfGreaterThan 1 from , arrow , parensIfGreaterThan 2 to ] >> Text.join ""))

        CA.TypeRecord pos extend attrs:

            attrsString =
                attrs
                    >> Dict.toList
                    >> List.sortBy Tuple.first
                    >> List.map (fn ( name & ty ): name .. " as " .. typeToText currentUmr meta ty)
                    >> Text.join ", "
            l = [
              , "{"
              , try extend as
                    Nothing:
                        ""

                    Just n:
                        n .. " with"
              , attrsString
              , "}"
              ]

            ( 0 & Text.join " " l)

        CA.TypeAlias pos ref ty2:
#            typeToPriorityAndText currentUmr meta ty2
            # TODO: once we can display a type on multiple lines, we should expand the alias?
            ( 0
              [#

                 , [ "<"
                   , name
                   , "="
                   , typeToString ty2
                   , ">"
                   ]
                     >> Text.join " "
              #]
            & typeRefToText currentUmr meta ref
            )


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


initNstate =
    as NormState
    { replacements = Dict.empty
    , next = 0
    }


intToName n acc =
    as Int: a: Text

    try n as
        0: "a"
        1: "b"
        2: "c"
        3: "d"
        4: "e"
        5: "f"
        _: Debug.todo "intToName"

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

    if r == 0:
        # `a` is zero, so the most significant symbol will start at `b` unless we remove 1
        # (per above, this is wrong)
        if acc == []:
            Text.fromList [ Char.fromCode head ]

        else
            Text.fromList (Char.fromCode (head - 1) :: acc)

    else
        intToName r (Char.fromCode head :: acc)
#]


newName state =
    as NormMonad Text
    ( intToName state.next []
    & { state with next = state.next + 1 }
    )


normalizeType t =
    as Type: Type
    t >> normType >> StateMonad.run initNstate >> Tuple.first


normalizeTypeAndTyvars tyOld tyvarsOld =
    as Type: Dict Text a: ( Type & Dict Text a )

    ( tyNew & state ) =
        tyOld
            >> normType
            >> StateMonad.run initNstate

    replace name =
        Dict.get name state.replacements
            >> Maybe.withDefault name
            >> Dict.insert

    tyvarsNew =
        Dict.foldl replace tyvarsOld Dict.empty

    ( tyNew & tyvarsNew )


normType ty =
    as Type: NormMonad Type
    try ty as
        CA.TypeConstant pos name args:
            (StateMonad.list_map normType args) >> then fn args_n:
            return << CA.TypeConstant pos name args_n

        CA.TypeVariable pos name:
            (normName name) >> then fn n:
            return << CA.TypeVariable pos n

        CA.TypeFunction pos from0 fromIsMut to0:
            (normType from0) >> then fn from1:
            (normType to0) >> then fn to1:
            return << CA.TypeFunction pos from1 fromIsMut to1

        CA.TypeRecord pos ext0 attrs0:
            (StateMonad.maybe_map normName ext0) >> then fn ext1:
            (StateMonad.dict_map (fn k: normType) attrs0) >> then fn attrs1:
            return << CA.TypeRecord pos ext1 attrs1

        CA.TypeAlias pos path t:
            (normType t) >> then fn t1:
            return << CA.TypeAlias pos path t1


normName name =
    as Text: NormMonad Text
    # Replace only names that are numbers ("22") or single letters ("a")
    if Text.length name > 1 and Text.toNumber name == Nothing:
        return name

    else
        get (fn x: x.replacements) >> then fn n2l:
        try Dict.get name n2l as
            Just replacement:
                return replacement

            Nothing:
                newName >> then fn n:

                addReplacement s =
                    as NormState: NormState
                    { s with replacements = Dict.insert name n s.replacements }

                (StateMonad.update addReplacement) >> then fn _:
                return n
