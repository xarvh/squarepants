#
# This module contains all the types that are necessary for the syntax.
#


p as Pos =
    Pos.N


umr as UMR =
    UMR Meta.Core "Core"


makeUsr as fn Name: USR =
    USR umr __


nameToType as fn Text, [CA.RawType]: CA.RawType =
    fn name, args:

    CA.TypeNamed p (makeUsr name) args


#
# Text
#


textUsr as USR =
    makeUsr "Text"


textType as CA.RawType =
    CA.TypeNamed p textUsr []


#
# Number
#


numberUsr as USR =
    makeUsr "Number"


numberType as CA.RawType =
    CA.TypeNamed p numberUsr []


#
# None
#


noneName as Name =
    "None"


noneType as CA.RawType =
    nameToType noneName []


noneCons as Dict Name [a] =
    Dict.ofOne noneName []


noneDef as CA.AliasDef =
    {
    , usr = makeUsr noneName
    , pars = []
    , type = CA.TypeUnion Pos.N noneCons
    , directTypeDeps = Set.empty
    }


#
# Bool
#


true as Name =
    "True"


false as Name =
    "False"


boolType as CA.RawType =
    nameToType "Bool" []


boolCons as Dict Name [a] =
    Dict.empty
    >> Dict.insert true [] __
    >> Dict.insert false [] __


boolDef as CA.AliasDef =
    {
    , usr = makeUsr "Bool"
    , pars = []
    , type = CA.TypeUnion p boolCons
    , directTypeDeps = Set.empty
    }




#
# List
#


listName as Name =
    "List"


nil as Name =
    "Nil"


cons as Name =
    "Cons"


listType as fn CA.RawType: CA.RawType =
    fn item:
    nameToType listName [ item ]


listDef as CA.AliasDef =
    usr =
        makeUsr listName

    item as CA.RawType =
        CA.TypeAnnotationVariable p "item"

    constructors =
        Dict.empty
        >> Dict.insert nil [] __
        >> Dict.insert cons [ item, listType item ] __


    {
    , usr
    , pars = [ At Pos.G "item" ]
    , type = CA.TypeUnion p constructors
    , directTypeDeps = Set.empty
    }


#
# All defs
#


allDefs as [CA.AliasDef] =
    [
    , noneDef
    , boolDef
    , listDef
#    , textDef
#    , numberDef
    ]

