#
# This module contains all the types that are necessary for the syntax.
#

p as Pos =
    Pos.'n


umr as UMR =
    'UMR Meta.'core "Core"


makeUsr as fn Name: USR =
    'USR umr __


nameToType as fn Text, [ CA.RawType ]: CA.RawType =
    fn name, args:
    CA.'typeNamed p (makeUsr name) args


defToType as fn CA.UnionDef, [ CA.RawType ]: CA.RawType =
    fn def, pars:
    CA.'typeNamed p def.usr pars


usrToVariable as fn USR: CA.Expression =
    fn u:
    CA.'variable p ('refGlobal u)


#
# Text
#

textDef as CA.UnionDef =
    {
    , constructors = Dict.empty
    , directTypeDeps = Set.empty
    , pars = []
    , usr = makeUsr "Text"
    }


text as CA.RawType =
    defToType textDef []


#
# Number
#

numberDef as CA.UnionDef =
    {
    , constructors = Dict.empty
    , directTypeDeps = Set.empty
    , pars = []
    , usr = makeUsr "Number"
    }


number as CA.RawType =
    defToType numberDef []


#
# None
#

noneConsName as Text =
    "'none"

noneTypeName as Text =
    "None"

noneType as CA.RawType =
    nameToType noneTypeName []

noneConsUsr as USR =
    makeUsr noneConsName

noneTypeUsr as USR =
    makeUsr noneTypeName


noneDef as CA.UnionDef =
    {
    , constructors = Dict.ofOne noneConsName { ins = [], out = noneType, pos = p, typeUsr = noneTypeUsr }
    , directTypeDeps = Set.empty
    , pars = []
    , usr = noneTypeUsr
    }


#
# Bool
#

trueName as Text = "'true"

trueUsr as USR =
    makeUsr trueName


falseName as Text = "'false"


falseUsr as USR =
    makeUsr falseName

boolName as Text = "Bool"

boolUsr as USR =
    makeUsr "Bool"

boolType as CA.RawType =
    nameToType boolName []

boolDef as CA.UnionDef =
    {
    , constructors =
        Dict.empty
        >> Dict.insert trueName { ins = [], out = boolType, pos = p, typeUsr = boolUsr } __
        >> Dict.insert falseName { ins = [], out = boolType, pos = p, typeUsr = boolUsr } __
    , directTypeDeps = Set.empty
    , pars = []
    , usr = boolUsr
    }


#
# List
#

nilName as Text = "'nil"

nilUsr as USR =
    makeUsr nilName

consName as Text = "'cons"

consUsr as USR =
    makeUsr consName


listName as Text = "List"

listType as fn CA.RawType: CA.RawType =
    fn item:
    nameToType listName [ item ]

listUsr as USR =
      makeUsr "List"

listDef as CA.UnionDef =

    item as CA.RawType =
        CA.'typeAnnotationVariable p "item"

    nilDef as CA.Constructor =
        {
        , ins = []
        , out = listType item
        , pos = p
        , typeUsr = listUsr
        }

    consDef as CA.Constructor =
        {
        , ins = [ item, listType item ]
        , out = listType item
        , pos = p
        , typeUsr = listUsr
        }

    {
    , constructors =
        Dict.empty
        >> Dict.insert nilName nilDef __
        >> Dict.insert consName consDef __
    , directTypeDeps = Set.empty
    , pars = [ "item" & Pos.'n ]
    , usr = listUsr
    }


#
# All defs
#

allDefs as [ CA.UnionDef ] =
    [
    , noneDef
    , boolDef
    , listDef
    , textDef
    , numberDef
    ]
