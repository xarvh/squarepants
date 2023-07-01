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


noneDef as CA.AliasDef =
    {
    , usr = makeUsr noneName
    , pars = []
    , type = Dict.ofOne noneName []
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


nil as Name =
    "Nil"


cons as Name =
    "Cons"


listType as fn CA.RawType: CA.RawType =
    fn item:
    nameToType "List" [ item ]


listDef as CA.AliasDef =
    usr =
        makeUsr "List"

    item as CA.RawType =
        CA.TypeAnnotationVariable p "item"

    nilDef as CA.Constructor =
       {
       , pos = p
       , ins = []
       , out = list item
       , typeUsr = usr
       }

    consDef as CA.Constructor =
        {
        , pos = p
        , ins = [ item, list item ]
        , out = list item
        , typeUsr = usr
        }

    {
    , usr
    , pars = [ At Pos.G "item" ]
    , constructors =
        Dict.empty
        >> Dict.insert "Nil" nilDef __
        >> Dict.insert "Cons" consDef __
    , directTypeDeps = Set.empty
    }


#
# All defs
#


allDefs as [CA.UnionDef] =
    [
    , noneDef
    , boolDef
    , listDef
    , textDef
    , numberDef
    ]

