

error pos description =
    as Pos: List Text: Res a
    Error.res pos fn _: description


alias GetType =
    Pos: Meta.UniqueSymbolReference: Res CA.TypeDef


#
# Expand aliases inside a type
#


expandInType ga ty =
    as GetType: CA.Type: Res CA.Type
    try ty as
        CA.TypeVariable pos name:
            Ok ty

        CA.TypeFunction pos from fromIsMutable to:
            expandInType ga from >> onOk fn f:
            expandInType ga to >> onOk fn t:
            Ok << CA.TypeFunction pos f fromIsMutable t

        CA.TypeRecord pos extensible attrs:
            attrs
                >> Dict.mapRes (fn k: expandInType ga)
                >> Result.map (CA.TypeRecord pos extensible)

        CA.TypeAlias pos path t:
            # it's easy to deal with, but it shouldn't happen O_O
            error pos [ "Did we apply aliases twice?" ]

        CA.TypeConstant pos usr args:
            List.mapRes (expandInType ga) args >> onOk fn replacedArgs:
            try ga pos usr as
                Err e:
                    Err e

                Ok (CA.TypeDefUnion un):
                    if List.length replacedArgs /= List.length un.args:
                        error pos [
                            , "union " .. Debug.toHuman un.usr .. " needs " .. Text.fromNumber (List.length un.args) .. " args,"
                            , "but was used with " .. Text.fromNumber (List.length replacedArgs) ]

                    else
                        replacedArgs
                            >> CA.TypeConstant pos usr
                            >> Ok

                Ok (CA.TypeDefAlias al):
                    if List.length al.args /= List.length replacedArgs:
                        error pos [ "alias " .. Debug.toHuman al.usr .. " needs " .. Text.fromNumber (List.length al.args) .. " args, but was used with " .. Text.fromNumber (List.length replacedArgs) ]

                    else
                        typeByArgName =
                            as Dict Name CA.Type
                            List.map2 (fn (At _ name) r: name & r ) al.args replacedArgs >> Dict.fromList

                        al.type
                            >> expandAliasVariables typeByArgName
                            >> CA.TypeAlias pos usr
                            >> Ok


expandAndValidateType ga rawTy =
    as GetType: CA.Type: Res CA.Type
    expandInType ga rawTy >> onOk fn expandedTy:
    try findMutableArgsThatContainFunctions Nothing expandedTy as
        []:
            Ok expandedTy

        errors:
            # TODO show the actual positions
            error (Pos.I 567)
                [ "Mutable arguments can't be or contain functions!"
                , errors >> List.map Debug.toHuman >> Text.join "\n"
                ]


findMutableArgsThatContainFunctions nonFunctionPos ty =
    as Maybe Pos: CA.Type: [ Pos & Pos ]
    try ty as
        CA.TypeConstant _ _ _:
            []

        CA.TypeVariable _ name:
            [# TODO
                if mutable:
                    Just "variable types can't be mutable"

               # except they can, they must be if we want to have functions
               capable of manipulating mutable containers

            #]
            []

        CA.TypeAlias _ path t:
            findMutableArgsThatContainFunctions nonFunctionPos t

        CA.TypeFunction functionPos from fromIsMutable to:
            [ try nonFunctionPos as
                Just constraintPos:
                    [ constraintPos & functionPos ]

                Nothing:
                    []
            , findMutableArgsThatContainFunctions (if fromIsMutable: Just functionPos else Nothing) from
            , findMutableArgsThatContainFunctions nonFunctionPos to
            ]
                >> List.concat

        CA.TypeRecord _ ext attrs:
            attrs
                >> Dict.values
                >> List.concatMap (findMutableArgsThatContainFunctions nonFunctionPos)


#
# Expand aliases inside other aliases
#


#
# TODO: have MakeCanonical calculate this as part of the alias' Deps
#
referencedAliases allAliases ty =
    as CA.All CA.AliasDef: CA.Type: Set Meta.UniqueSymbolReference
    try ty as
        CA.TypeConstant pos usr args:
            init =
                if Dict.member usr allAliases:
                    Set.singleton usr
                else
                    Set.empty

            List.foldl (fn ar: Dict.join (referencedAliases allAliases ar)) args (Set.singleton usr)

        CA.TypeVariable pos name:
            Dict.empty

        CA.TypeFunction pos from maybeMut to:
            Dict.join (referencedAliases allAliases from) (referencedAliases allAliases to)

        CA.TypeRecord pos extensible attrs:
            Dict.foldl (fn name t: Dict.join (referencedAliases allAliases t)) attrs Dict.empty

        CA.TypeAlias pos path t:
            referencedAliases allAliases t



expandAndInsertAlias allTypes al expandedTypes =
    as CA.All CA.TypeDef: CA.AliasDef: CA.All CA.TypeDef: Res (CA.All CA.TypeDef)

    getAlias pos usr =
        as GetType

        try Dict.get usr expandedTypes as
            Just type:
                Ok type

            Nothing:
                try Dict.get usr allTypes as
                    Nothing:
                        error pos [ "Undefined type: `" .. Debug.toHuman usr .. "`" ]

                    Just (CA.TypeDefAlias a):
                        Debug.todo << "expandAndInsertAlias should-not-happen: " .. Debug.toHuman usr

                    Just (CA.TypeDefUnion u):
                        Ok << CA.TypeDefUnion u


    expandAndValidateType getAlias al.type >> onOk fn type:
    Ok << Dict.insert al.usr (CA.TypeDefAlias { al with type }) expandedTypes


expandAliasVariables typeByArgName ty =
    as Dict Name CA.Type: CA.Type: CA.Type
    try ty as
        CA.TypeVariable pos name:
            try Dict.get name typeByArgName as
                Nothing:
                    ty

                Just t:
                    t

        CA.TypeFunction pos from fromIsMutable to:
            CA.TypeFunction pos
                (expandAliasVariables typeByArgName from)
                fromIsMutable
                (expandAliasVariables typeByArgName to)

        CA.TypeRecord pos extensible attrs:
            CA.TypeRecord pos
                extensible
                (Dict.map (fn k: expandAliasVariables typeByArgName) attrs)

        CA.TypeConstant pos usr args:
            args
                >> List.map (expandAliasVariables typeByArgName)
                >> CA.TypeConstant pos usr

        CA.TypeAlias pos usr t:
            CA.TypeAlias pos usr (expandAliasVariables typeByArgName t)


#
# Apply aliases to unions
#


getTypeForUnion allTypes expandedTypes pos usr =
    as CA.All CA.TypeDef: CA.All CA.TypeDef: GetType

    # Try to find the type here first
    # All aliases will be here and will be expanded
    # Not all unions will be here tho, because we're putting them there now
    try Dict.get usr expandedTypes as
        Just t:
            Ok t

        Nothing:
            # Union types that haven't yet been expanded will be here
            # It is not a problem that the union type is not expande
            try Dict.get usr allTypes as
                Just t:
                    Ok t

                Nothing:
                    error pos [ "Undefined type usr: `" .. Debug.toHuman usr .. "`" ]



expandAndInsertUnion allTypes usr typeDef expandedTypes =
    as CA.All CA.TypeDef: Meta.UniqueSymbolReference: CA.TypeDef: CA.All CA.TypeDef: Res (CA.All CA.TypeDef)

    try typeDef as
        CA.TypeDefAlias _:
            Ok expandedTypes

        CA.TypeDefUnion u:
            gt =
                as GetType
                getTypeForUnion allTypes expandedTypes

            mapConstructor name c =
                as Name: CA.Constructor: Res CA.Constructor
                # TODO these are redundant, do we need both?
                expandAndValidateType gt c.type >> onOk fn type:
                List.mapRes (expandAndValidateType gt) c.args >> onOk fn args:
                Ok << { c with type, args }

            Dict.mapRes mapConstructor u.constructors >> onOk fn cs:
            Ok << Dict.insert usr (CA.TypeDefUnion { u with constructors = cs }) expandedTypes


#
# Build main dict with all resolved aliases and expanded unions
#


insertModuleTypes module allTypes =
    as CA.Module: CA.All CA.TypeDef: CA.All CA.TypeDef

    allTypes
        >> Dict.foldl (fn name def: Dict.insert def.usr << CA.TypeDefAlias def) module.aliasDefs
        >> Dict.foldl (fn name def: Dict.insert def.usr << CA.TypeDefUnion def) module.unionDefs


expandAllTypes allTypes =
    as CA.All CA.TypeDef: Res (CA.All CA.TypeDef)

    allAliases =
        as CA.All CA.AliasDef

        insertAlias usr typeDef acc =
            try typeDef as
                CA.TypeDefAlias a: Dict.insert usr a acc
                _: acc

        Dict.foldl insertAlias allTypes Dict.empty


    orderedAliasesResult =
        RefHierarchy.reorder (fn al: referencedAliases allAliases al.type) allAliases

    try orderedAliasesResult as
        Err circular:
            error (Pos.I 121) [
                , "circular alias: "
                , circular
                #   >> List.filterMap (fn usr: Dict.get usr als)
                    >> List.map Debug.toHuman
                    >> Text.join " <- "
                ]

        Ok oa:
            Dict.empty
                >> List.foldlRes (expandAndInsertAlias allTypes) oa
                >> onOk (Dict.foldlRes (expandAndInsertUnion allTypes) allTypes)


#
# Apply aliases to annotations
#
expandAnnotation allExpandedTypes type =
    as CA.All CA.TypeDef: CA.Type: Res CA.Type

    gt pos usr =
        as GetType
        try Dict.get usr allExpandedTypes as
            Just t: Ok t
            Nothing: error pos [ "Undefined type usr: `" .. Debug.toHuman usr .. "`" ]

    expandAndValidateType gt type

