#
# This module contains all the types that are necessary for the syntax.
#


p =
    as Pos
    Pos.N


usr name =
    as Name: Meta.UniqueSymbolReference

    Meta.USR Meta.Core [] name


nameToType name =
    as Text: [CA.Type]: CA.Type

    name
        >> usr
        >> CA.Foreign
        >> CA.TypeConstant p


defToType { name = At _ name } =
    as CA.UnionDef: [CA.Type]: CA.Type

    nameToType name


nameToRef name =
    as Name: CA.Ref

    CA.Foreign << usr name


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
    , constructors = Dict.singleton noneName [ none ]
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
            >> Dict.insert "True" [ bool ]
            >> Dict.insert "False" [ bool ]
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
        CA.TypeAnnotatedVar p "item"

    consType =
        CA.TypeFunction p item False
            (CA.TypeFunction p (list item) False
                (list item)
            )

    { name = At p "List"
    , args = [ "item" ]
    , constructors =
        Dict.empty
            >> Dict.insert "Nil" [ list item ]
            >> Dict.insert "Cons" [ consType ]
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

