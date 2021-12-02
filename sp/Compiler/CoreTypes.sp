#
# This module contains all the types that are necessary for the syntax.
#


p =
    as Pos
    Pos.N

umr =
    as Meta.UniqueModuleReference
    Meta.UMR Meta.Core "SPCore"

usr name =
    as Name: Meta.UniqueSymbolReference

    Meta.USR umr name


nameToType name =
    as Text: [CA.Type]: CA.Type

    name
        >> usr
        >> CA.RefRoot
        >> CA.TypeConstant p


defToType { name = At _ name } =
    as CA.UnionDef: [CA.Type]: CA.Type

    nameToType name


nameToRef name =
    as Name: CA.Ref

    CA.RefRoot << usr name


refToVariable ref =
    as CA.Ref: CA.Expression

    CA.Variable p { attrPath = [], ref }



#
# Text
#


textDef =
    as CA.UnionDef
    { name = At p "Text"
    , args = []
    , constructors = Dict.empty
    }


text =
    as CA.Type
    defToType textDef []


#
# Number
#


numberDef =
    as CA.UnionDef
    { name = At p "Number"
    , args = []
    , constructors = Dict.empty
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
    as CA.Ref
    nameToRef noneName


noneDef =
    as CA.UnionDef

    { name = At p noneName
    , args = []
    , constructors = Dict.singleton noneName (p & [])
    }


#
# Bool
#


true =
    as CA.Ref
    nameToRef "True"


false =
    as CA.Ref
    nameToRef "False"


bool =
    as CA.Type
    nameToType "Bool" []


boolDef =
    as CA.UnionDef
    { name = At p "Bool"
    , args = []
    , constructors =
        Dict.empty
            >> Dict.insert "True" (p & [])
            >> Dict.insert "False" (p & [])
    }


#
# List
#


nil =
    as CA.Ref
    nameToRef "Nil"


cons =
    as CA.Ref
    nameToRef "Cons"


list item =
    as CA.Type: CA.Type
    nameToType "List" [ item ]


listDef =
    as CA.UnionDef

    item =
        #CA.TypeAnnotatedVar p "item"
        CA.TypeVariable p "item"

    { name = At p "List"
    , args = [ "item" ]
    , constructors =
        Dict.empty
            >> Dict.insert "Nil" (p & [])
            >> Dict.insert "Cons" (p & [ item, list item ])
    }


#
# All defs
#


defs =
    as [CA.UnionDef]
    [ noneDef
    , boolDef
    , listDef
    , textDef
    , numberDef
    ]

