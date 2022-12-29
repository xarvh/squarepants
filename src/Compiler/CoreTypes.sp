#
# This module contains all the types that are necessary for the syntax.
#


p as Pos =
    Pos.N


umr as UMR =
    UMR Meta.Core "Core"


makeUsr as Name: USR =
    USR umr


nameToType as Text: [CA.Type]: CA.Type =
    name: args:

    args
    >> CA.TypeNamed (makeUsr name) (CA.UniIsFixed Imm)
    >> CA.Type p


defToType as CA.UnionDef: [CA.Type]: CA.Type =
    def: pars:
    CA.Type p (CA.TypeNamed def.usr (CA.UniIsFixed Imm) pars)


usrToVariable as USR: CA.Expression =
    u:
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


text as CA.Type =
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


number as CA.Type =
    defToType numberDef []


#
# None
#


noneName =
    "None"


none as CA.Type =
    nameToType noneName []

noneValue as USR =
    makeUsr noneName


noneDef as CA.UnionDef =
    usr = makeUsr noneName
    {
    , usr
    , pars = []
    , constructors = Dict.singleton noneName { pos = p, pars = [], type = none, typeUsr = usr }
    , directTypeDeps = Set.empty
    }


#
# Bool
#


true as USR =
    makeUsr "True"


false as USR =
    makeUsr "False"


bool as CA.Type =
    nameToType "Bool" []


boolDef as CA.UnionDef =
    usr = makeUsr "Bool"
    {
    , usr
    , pars = []
    , constructors =
        Dict.empty
            >> Dict.insert "True" { pos = p, pars = [], type = bool, typeUsr = usr }
            >> Dict.insert "False" { pos = p, pars = [], type = bool, typeUsr = usr }
    , directTypeDeps = Set.empty
    }


#
# List
#


nil as USR =
    makeUsr "Nil"


cons as USR =
    makeUsr "Cons"


list as CA.Type: CA.Type =
    item:
    nameToType "List" [ item ]


listDef as CA.UnionDef =
    usr =
        makeUsr "List"

    item as CA.Type =
        CA.Type p << CA.TypeAnnotationVariable Imm "item"

    pars as [CA.Type] =
        [ item, list item ]

    type as CA.Type =
        CA.Type p << CA.TypeFn [Spend & item, Spend & list item] (list item)

    nilDef as CA.Constructor =
       {
       , pos = p
       , pars = []
       , type = list item
       , typeUsr = usr
       }

    consDef as CA.Constructor =
        {
        , pos = p
        , pars
        , type
        , typeUsr = usr
        }

    {
    , usr
    , pars = [ At Pos.G "item" ]
    , constructors =
        Dict.empty
            >> Dict.insert "Nil" nilDef
            >> Dict.insert "Cons" consDef
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

