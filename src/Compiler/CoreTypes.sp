#
# This module contains all the types that are necessary for the syntax.
#


p as Pos =
    Pos.N


umr as Meta.UniqueModuleReference =
    Meta.UMR Meta.Core "Core"


makeUsr as Name: Meta.UniqueSymbolReference =
    Meta.USR umr


nameToType as Text: [CA.Type]: CA.Type =
    name:
    name
    >> makeUsr
    >> CA.TypeNamed p


defToType as CA.UnionDef: [CA.Type]: CA.Type =
    def:
    CA.TypeNamed p def.usr


usrToVariable as Meta.UniqueSymbolReference: CA.Expression =
    u:
    CA.Variable p (CA.RefGlobal u)


#
# Text
#


textDef as CA.UnionDef = {
    , usr = makeUsr "Text"
    , args = []
    , constructors = Dict.empty
    , directTypeDeps = Set.empty
    }


text as CA.Type =
    defToType textDef []


#
# Number
#


numberDef as CA.UnionDef = {
    , usr = makeUsr "Number"
    , args = []
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

noneValue as Meta.UniqueSymbolReference =
    makeUsr noneName


noneDef as CA.UnionDef =
    usr = makeUsr noneName
    {
    , usr
    , args = []
    , constructors = Dict.singleton noneName { pos = p, args = [], type = none, typeUsr = usr }
    , directTypeDeps = Set.empty
    }


#
# Bool
#


true as Meta.UniqueSymbolReference =
    makeUsr "True"


false as Meta.UniqueSymbolReference =
    makeUsr "False"


bool as CA.Type =
    nameToType "Bool" []


boolDef as CA.UnionDef =
    usr = makeUsr "Bool"
    {
    , usr
    , args = []
    , constructors =
        Dict.empty
            >> Dict.insert "True" { pos = p, args = [], type = bool, typeUsr = usr }
            >> Dict.insert "False" { pos = p, args = [], type = bool, typeUsr = usr }
    , directTypeDeps = Set.empty
    }


#
# List
#


nil as Meta.UniqueSymbolReference =
    makeUsr "Nil"


cons as Meta.UniqueSymbolReference =
    makeUsr "Cons"


list as CA.Type: CA.Type =
    item:
    nameToType "List" [ item ]


listDef as CA.UnionDef =
    usr =
        makeUsr "List"

    item as CA.Type =
        CA.TypeAnnotationVariable p "item"

    args as [CA.Type] =
        [ item, list item ]

    type as CA.Type =
        List.forReversed args (ar: ty: CA.TypeFunction p [LambdaNormal & ar] ty) (list item)

    consDef as CA.Constructor = {
        , pos = p
        , args
        , type
        , typeUsr = usr
        }

    { usr
    , args = [ "item" ]
    , constructors =
        Dict.empty
            >> Dict.insert "Nil" { pos = p, args = [], type = list item, typeUsr = usr }
            >> Dict.insert "Cons" consDef
    , directTypeDeps = Set.empty
    }


#
# All defs
#


allDefs as [CA.UnionDef] = [
    , noneDef
    , boolDef
    , listDef
    , textDef
    , numberDef
    ]

