

error as Pos: List Text: Res a =
    pos: description:
    Error.res pos _: description


alias GetType =
    Pos: Meta.UniqueSymbolReference: Res CA.TypeDef


#
# Expand aliases inside a type
#


expandInType as GetType: CA.Type: Res CA.Type =
    ga: ty:
    try ty as
        CA.TypeVariable pos name flags:
            Ok ty

        CA.TypeMutable pos t:
            expandInType ga t >> onOk ety:
            Ok << CA.TypeMutable pos ety

        CA.TypeFunction pos from fromIsMutable to:
            expandInType ga from >> onOk f:
            expandInType ga to >> onOk t:
            Ok << CA.TypeFunction pos f fromIsMutable t

        CA.TypeRecord pos attrs:
            attrs
            >> Dict.mapRes (k: expandInType ga)
            >> Result.map (CA.TypeRecord pos)
            >> Result.map t: if CA.typeContainsUniques t then CA.TypeMutable pos t else t

        CA.TypeRecordExt pos name flags attrs:
            attrs
            >> Dict.mapRes (k: expandInType ga)
            >> Result.map (CA.TypeRecordExt pos name flags)

        CA.TypeAlias pos path t:
            # it's easy to deal with, but it shouldn't happen O_O
            error pos [ "Did we apply aliases twice?" ]

        CA.TypeConstant pos usr args:
            List.mapRes (expandInType ga) args >> onOk replacedArgs:
            try ga pos usr as
                Err e:
                    Err e

                Ok (CA.TypeDefUnion un):
                    if List.length replacedArgs /= List.length un.args then
                        error pos [
                            , "union " .. toHuman un.usr .. " needs " .. Text.fromNumber (List.length un.args) .. " args,"
                            , "but was used with " .. Text.fromNumber (List.length replacedArgs) ]

                    else
                        replacedArgs
                            >> CA.TypeConstant pos usr
                            >> Ok

                Ok (CA.TypeDefAlias al):
                    if List.length al.args /= List.length replacedArgs then
                        error pos [ "alias " .. toHuman al.usr .. " needs " .. Text.fromNumber (List.length al.args) .. " args, but was used with " .. Text.fromNumber (List.length replacedArgs) ]

                    else
                        typeByArgName as Dict Name CA.Type =
                            List.map2 ((At _ name): r: name & r ) al.args replacedArgs >> Dict.fromList

                        al.type
                            >> expandAliasVariables typeByArgName
                            >> CA.TypeAlias pos usr
                            >> Ok


#findMutableArgsThatContainFunctions as Maybe Pos: CA.Type: [ Pos & Pos ] =
#    nonFunctionPos: ty:
#    try ty as
#        CA.TypeConstant _ _ _:
#            []
#
#        CA.TypeMutable _ t:
#            findMutableArgsThatContainFunctions nonFunctionPos t
#
#        CA.TypeVariable _ name flags:
#            [# TODO
#                if mutable then
#                    Just "variable types can't be mutable"
#
#               # except they can, they must be if we want to have functions
#               capable of manipulating mutable containers
#
#            #]
#            []
#
#        CA.TypeAlias _ path t:
#            findMutableArgsThatContainFunctions nonFunctionPos t
#
#        CA.TypeFunction functionPos from lambdaModifier to:
#            [ try nonFunctionPos as
#                Just constraintPos:
#                    [ constraintPos & functionPos ]
#
#                Nothing:
#                    []
#            , findMutableArgsThatContainFunctions nonFunctionPos from
#            , findMutableArgsThatContainFunctions nonFunctionPos to
#            ]
#                >> List.concat
#
#        CA.TypeRecord _ attrs:
#            attrs
#            >> Dict.values
#            >> List.concatMap (findMutableArgsThatContainFunctions nonFunctionPos)
#
#        CA.TypeRecordExt _ name flags attrs:
#            attrs
#            >> Dict.values
#            >> List.concatMap (findMutableArgsThatContainFunctions nonFunctionPos)


#
# Expand aliases inside other aliases
#


#
# TODO: have MakeCanonical calculate this as part of the alias' Deps
#
referencedAliases as CA.All CA.AliasDef: CA.Type: Set Meta.UniqueSymbolReference =
    allAliases: ty:
    try ty as
        CA.TypeConstant pos usr args:
            init =
                if Dict.member usr allAliases then
                    Set.singleton usr
                else
                    Set.empty

            List.for args (ar: Dict.join (referencedAliases allAliases ar)) (Set.singleton usr)

        CA.TypeVariable pos name flags:
            Dict.empty

        CA.TypeFunction pos from maybeMut to:
            Dict.join (referencedAliases allAliases from) (referencedAliases allAliases to)

        CA.TypeRecord pos attrs:
            Dict.for attrs (name: t: Dict.join (referencedAliases allAliases t)) Dict.empty

        CA.TypeRecordExt pos name flags attrs:
            Dict.for attrs (n: t: Dict.join (referencedAliases allAliases t)) Dict.empty

        CA.TypeAlias pos path t:
            referencedAliases allAliases t

        CA.TypeMutable pos t:
            referencedAliases allAliases t




expandAndInsertAlias as CA.All CA.TypeDef: CA.AliasDef: CA.All CA.TypeDef: Res (CA.All CA.TypeDef) =
    allTypes: al: expandedTypes:

    getAlias as GetType =
        pos: usr:

        try Dict.get usr expandedTypes as
            Just type:
                Ok type

            Nothing:
                try Dict.get usr allTypes as
                    Nothing:
                        error pos [ "Undefined type: `" .. toHuman usr .. "`" ]

                    Just (CA.TypeDefAlias a):
                        todo << "expandAndInsertAlias should-not-happen: " .. toHuman usr

                    Just (CA.TypeDefUnion u):
                        Ok << CA.TypeDefUnion u


    expandInType getAlias al.type >> onOk type:
    Ok << Dict.insert al.usr (CA.TypeDefAlias { al with type }) expandedTypes


expandAliasVariables as Dict Name CA.Type: CA.Type: CA.Type =
    typeByArgName: ty:

    try ty as

        CA.TypeVariable pos name flags:
            try Dict.get name typeByArgName as
                Nothing:
                    ty

                Just t:
                    if not flags.allowFunctions and Compiler/TypeCheck.typeContainsFunctions t then
                        Debug.todo << "can't expand type \n\n" .. toHuman ty .. "\n\n with type \n\n" .. toHuman t .. "\n\nbecause the latter contains a functions"
                    else
                      if not flags.allowUniques and CA.typeContainsUniques t then
                          Debug.todo << "can't expand type \n\n" .. toHuman ty .. "\n\n with type \n\n" .. toHuman t .. "\n\nbecause the latter contains a mutable"
                      else
                        t

        CA.TypeFunction pos from fromIsMutable to:
            CA.TypeFunction pos
                (expandAliasVariables typeByArgName from)
                fromIsMutable
                (expandAliasVariables typeByArgName to)

        CA.TypeRecord pos attrs:
            CA.TypeRecord pos
                (Dict.map (k: expandAliasVariables typeByArgName) attrs)

        CA.TypeRecordExt pos name flags attrs:
            CA.TypeRecordExt pos name flags
                (Dict.map (k: expandAliasVariables typeByArgName) attrs)

        CA.TypeConstant pos usr args:
            args
                >> List.map (expandAliasVariables typeByArgName)
                >> CA.TypeConstant pos usr

        CA.TypeAlias pos usr t:
            CA.TypeAlias pos usr (expandAliasVariables typeByArgName t)


#
# Apply aliases to unions
#


getTypeForUnion as CA.All CA.TypeDef: CA.All CA.TypeDef: GetType =
    allTypes: expandedTypes: pos: usr:

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
                    error pos [ "Undefined type usr: `" .. toHuman usr .. "`" ]



expandAndInsertUnion as CA.All CA.TypeDef: Meta.UniqueSymbolReference: CA.TypeDef: CA.All CA.TypeDef: Res (CA.All CA.TypeDef) =
    allTypes: usr: typeDef: expandedTypes:

    try typeDef as
        CA.TypeDefAlias _:
            Ok expandedTypes

        CA.TypeDefUnion u:
            gt as GetType =
                getTypeForUnion allTypes expandedTypes

            mapConstructor as Name: CA.Constructor: Res CA.Constructor =
                name: c:
                # TODO these are redundant, do we need both?
                expandInType gt c.type >> onOk type:
                List.mapRes (expandInType gt) c.args >> onOk args:
                Ok << { c with type, args }

            Dict.mapRes mapConstructor u.constructors >> onOk cs:
            Ok << Dict.insert usr (CA.TypeDefUnion { u with constructors = cs }) expandedTypes


#
# Build main dict with all resolved aliases and expanded unions
#


insertModuleTypes as CA.Module: CA.All CA.TypeDef: CA.All CA.TypeDef =
    module: allTypes:
    allTypes
        >> Dict.for module.aliasDefs (name: def: Dict.insert def.usr << CA.TypeDefAlias def)
        >> Dict.for module.unionDefs (name: def: Dict.insert def.usr << CA.TypeDefUnion def)


expandAllTypes as CA.All CA.TypeDef: Res (CA.All CA.TypeDef) =
    allTypes:

    allAliases as CA.All CA.AliasDef =

        insertAlias =
            usr: typeDef: acc:
            try typeDef as
                CA.TypeDefAlias a: Dict.insert usr a acc
                _: acc

        Dict.for allTypes insertAlias Dict.empty


    circulars & orderedAliasRefs =
        RefHierarchy.reorder (al: referencedAliases allAliases al.type) allAliases

    if circulars /= [] then
        circularToError =
            circular:
            Error.Simple (Pos.I 121) _: [
                , "circular alias: "
                , circular
                #   >> List.filterMap (fn usr: Dict.get usr als)
                    >> List.map toHuman
                    >> Text.join " <- "
                ]

        circulars
            >> List.map circularToError
            >> Error.Nested
            >> Err

    else
        oa as [CA.AliasDef] =
          orderedAliasRefs
              >> List.filterMap (ref: Dict.get ref allAliases)

        Dict.empty
        >> List.forRes oa (expandAndInsertAlias allTypes)
        >> onOk (Dict.forRes allTypes (expandAndInsertUnion allTypes))


#
# Apply aliases to annotations
#
expandAnnotation as CA.All CA.TypeDef: CA.Type: Res CA.Type =
    allExpandedTypes: type:

    gt as GetType =
        pos: usr:
        try Dict.get usr allExpandedTypes as
            Just t: Ok t
            Nothing: error pos [ "Undefined type usr: `" .. toHuman usr .. "`" ]

    expandInType gt type

