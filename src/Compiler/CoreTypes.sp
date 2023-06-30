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


defToType as fn CA.UnionDef, [CA.RawType]: CA.RawType =
    fn def, pars:
    CA.TypeNamed p def.usr pars


usrToVariable as fn USR: CA.Expression =
    fn u:
    CA.Variable p (RefGlobal u)


#
# Text
#


textDef as CA.UnionDef =
    {
    , usr = makeUsr "Text"
    , pars = []
    , constructors = Dict.empty
    , directTypeDeps = Set.empty
    }


text as CA.RawType =
    defToType textDef []


#
# Number
#


numberDef as CA.UnionDef =
    {
    , usr = makeUsr "Number"
    , pars = []
    , constructors = Dict.empty
    , directTypeDeps = Set.empty
    }


number as CA.RawType =
    defToType numberDef []


#
# None
#


noneName =
    "None"


none as CA.RawType =
    nameToType noneName []

noneValue as Name =
    noneName


noneDef as CA.UnionDef =
    usr = makeUsr noneName
    {
    , usr
    , pars = []
    , constructors = Dict.ofOne noneName { pos = p, ins = [], out = none, typeUsr = usr }
    , directTypeDeps = Set.empty
    }


#
# Bool
#


true as USR =
    makeUsr "True"


false as USR =
    makeUsr "False"


bool as CA.RawType =
    nameToType "Bool" []


boolDef as CA.UnionDef =
    usr = makeUsr "Bool"
    {
    , usr
    , pars = []
    , constructors =
        Dict.empty
        >> Dict.insert "True" { pos = p, ins = [], out = bool, typeUsr = usr } __
        >> Dict.insert "False" { pos = p, ins = [], out = bool, typeUsr = usr } __
    , directTypeDeps = Set.empty
    }


#
# List
#


nil as Name =
    "Nil"


cons as Name =
    "Cons"


list as fn CA.RawType: CA.RawType =
    fn item:
    nameToType "List" [ item ]


listDef as CA.UnionDef =
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

