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


listUsr as USR =
    makeUsr listName


nil as Name =
    "Nil"


cons as Name =
    "Cons"


listCons as fn item: Dict Name [item] =
    fn item:

    Dict.empty
    >> Dict.insert nil [] __
    >> Dict.insert cons [ item, CA.TypeNamed p listUsr [ item ] ] __


listType as fn CA.RawType: CA.RawType =
    fn item:
    CA.TypeUnion p (listCons item)


listDef as CA.AliasDef =
    {
    , usr = listUsr
    , pars = [ At p "item" ]
    , type = listType (CA.TypeAnnotationVariable p "item")
    , directTypeDeps = Set.empty
    }


#
# All defs
#


aliases as ByUsr CA.AliasDef =
    [
    , noneDef
    , boolDef
    , listDef
#    , textDef
#    , numberDef
    ]
    >> List.indexBy (fn a: a.usr) __

