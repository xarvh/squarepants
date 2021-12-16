#
# This module contains all the types that are necessary for the syntax.
#


p =
    as Pos
    Pos.N


umr =
    as Meta.UniqueModuleReference
    Meta.UMR Meta.Core "SPCore"


makeUsr name =
    as Name: Meta.UniqueSymbolReference

    Meta.USR umr name


nameToType name =
    as Text: [CA.Type]: CA.Type

    name
        >> makeUsr
        >> CA.TypeConstant p


defToType def =
    as CA.UnionDef: [CA.Type]: CA.Type

    CA.TypeConstant p def.usr


#
# Text
#


textDef =
    as CA.UnionDef
    {
    , usr = makeUsr "Text"
    , args = []
    , constructors = Dict.empty
    , directTypeDeps = Set.empty
    }


text =
    as CA.Type
    defToType textDef []


#
# Number
#


numberDef =
    as CA.UnionDef
    { usr = makeUsr "Number"
    , args = []
    , constructors = Dict.empty
    , directTypeDeps = Set.empty
    }


number =
    as CA.Type
    defToType numberDef []


#
# None
#


noneName =
    "None"


none =
    as CA.Type
    nameToType noneName []

noneValue =
    as Meta.UniqueSymbolReference
    makeUsr noneName


noneDef =
    as CA.UnionDef

    { usr = makeUsr noneName
    , args = []
    , constructors = Dict.singleton noneName { pos = p, args = [], type = none }
    , directTypeDeps = Set.empty
    }


#
# Bool
#


true =
    as Meta.UniqueSymbolReference
    makeUsr "True"


false =
    as Meta.UniqueSymbolReference
    makeUsr "False"


bool =
    as CA.Type
    nameToType "Bool" []


boolDef =
    as CA.UnionDef
    { usr = makeUsr "Bool"
    , args = []
    , constructors =
        Dict.empty
            >> Dict.insert "True" { pos = p, args = [], type = bool }
            >> Dict.insert "False" { pos = p, args = [], type = bool }
    , directTypeDeps = Set.empty
    }


#
# List
#


nil =
    as Meta.UniqueSymbolReference
    makeUsr "Nil"


cons =
    as Meta.UniqueSymbolReference
    makeUsr "Cons"


list item =
    as CA.Type: CA.Type
    nameToType "List" [ item ]


listDef =
    as CA.UnionDef

    item =
        #CA.TypeAnnotatedVar p "item"
        CA.TypeVariable p "item"

    consDef =
        as CA.Constructor
        {
        , pos = p
        , args = [ item, list item ]
        , type = List.foldr (fn ar ty: CA.TypeFunction p ar False ty) [ item, list item ] (list item)
        }

    { usr = makeUsr "List"
    , args = [ "item" ]
    , constructors =
        Dict.empty
            >> Dict.insert "Nil" { pos = p, args = [], type = list item }
            >> Dict.insert "Cons" consDef
    , directTypeDeps = Set.empty
    }


#
# All defs
#


allDefs =
    as [CA.UnionDef]
    [
    , noneDef
    , boolDef
    , listDef
    , textDef
    , numberDef
    ]

