#
# This module contains all the definitions that are necessary for the syntax.
#

importsDir as Text =
    ""


sourceDir as Text =
    "src"


pathId as Int =
    0


importsPath as Meta.ImportsPath =
    Meta.'importsPath Meta.'core importsDir


makeUmr as fn Text: UMR =
    'UMR Meta.'core pathId __


umr as UMR =
    makeUmr "Core"


usr as fn Name: USR =
    'USR umr __


nameToType as fn Text, [ CA.RawType ]: CA.RawType =
    fn name, args:
    CA.'typeNamed Pos.'n (usr name) args


defToType as fn CA.VariantTypeDef, [ CA.RawType ]: CA.RawType =
    fn def, pars:
    CA.'typeNamed Pos.'n def.usr pars


usrToVariable as fn USR: CA.Expression =
    fn u:
    CA.'variable Pos.'n ('refGlobal u)


#
# Text
#

textUsr as USR =
    usr "Text"


textDef as CA.VariantTypeDef =
    {
    , constructors = Dict.empty
    , pars = []
    , usr = textUsr
    }


text as CA.RawType =
    defToType textDef []


#
# Number
#

numberUsr as USR =
    usr "Number"


numberDef as CA.VariantTypeDef =
    {
    , constructors = Dict.empty
    , pars = []
    , usr = numberUsr
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
    usr noneConsName


noneTypeUsr as USR =
    usr noneTypeName


noneConsDef as CA.ConstructorDef =
    {
    , constructorUsr = noneConsUsr
    , directDeps = Dict.ofOne noneTypeUsr 'typeDependency
    , ins = []
    , name = noneConsName
    , out = noneType
    , pos = Pos.'n
    , variantTypeUsr = noneTypeUsr
    }


noneTypeDef as CA.VariantTypeDef =
    {
    , constructors = Dict.ofOne noneConsName noneConsDef
    , pars = []
    , usr = noneTypeUsr
    }


#
# Bool
#

trueName as Text =
    "'true"


trueUsr as USR =
    usr trueName


falseName as Text =
    "'false"


falseUsr as USR =
    usr falseName


boolName as Text =
    "Bool"


boolUsr as USR =
    usr "Bool"


boolType as CA.RawType =
    nameToType boolName []


trueDef as CA.ConstructorDef =
    {
    , constructorUsr = trueUsr
    , directDeps = Dict.ofOne boolUsr 'typeDependency
    , ins = []
    , name = trueName
    , out = boolType
    , pos = Pos.'n
    , variantTypeUsr = boolUsr
    }


falseDef as CA.ConstructorDef =
    {
    , constructorUsr = falseUsr
    , directDeps = Dict.ofOne boolUsr 'typeDependency
    , ins = []
    , name = falseName
    , out = boolType
    , pos = Pos.'n
    , variantTypeUsr = boolUsr
    }


boolDef as CA.VariantTypeDef =
    {
    , constructors =
        Dict.empty
        >> Dict.insert trueName trueDef __
        >> Dict.insert falseName falseDef __
    , pars = []
    , usr = boolUsr
    }


#
# List
#

nilName as Text =
    "'nil"


nilUsr as USR =
    usr nilName


consName as Text =
    "'cons"


consUsr as USR =
    usr consName


listName as Text =
    "List"


listType as fn CA.RawType: CA.RawType =
    fn item:
    nameToType listName [ item ]


listUsr as USR =
    usr "List"


listItem as CA.RawType =
    CA.'typeAnnotationVariable Pos.'n "item"


nilDef as CA.ConstructorDef =
    {
    , constructorUsr = nilUsr
    , directDeps = Dict.ofOne listUsr 'typeDependency
    , ins = []
    , name = nilName
    , out = listType listItem
    , pos = Pos.'n
    , variantTypeUsr = listUsr
    }


consDef as CA.ConstructorDef =
    {
    , constructorUsr = consUsr
    , directDeps = Dict.ofOne listUsr 'typeDependency
    , ins = [ listItem, listType listItem ]
    , name = consName
    , out = listType listItem
    , pos = Pos.'n
    , variantTypeUsr = listUsr
    }


listDef as CA.VariantTypeDef =
    {
    , constructors =
        Dict.empty
        >> Dict.insert nilName nilDef __
        >> Dict.insert consName consDef __
    , pars = [ "item" & Pos.'n ]
    , usr = listUsr
    }


#
# Type helpers
#
# TODO Do I need types here? It would be nicer if all the ops where just syntactic sugar for function calls
#

tyVar as fn Name: CA.RawType =
    CA.'typeAnnotationVariable Pos.'n __


tyFn as fn [ CA.RawType ], CA.RawType: CA.RawType =
    fn pars, to:
    CA.'typeFn Pos.'n (List.map (fn p: CA.'parSp (toImm p)) pars) (toImm to)


typeBinop as fn CA.RawType, CA.RawType, CA.RawType: CA.RawType =
    fn left, right, return:
    tyFn [ left, right ] return


typeBinopUnique as fn CA.RawType: CA.RawType =
    fn ty:
    CA.'typeFn Pos.'n [ CA.'parSp (toImm ty), CA.'parSp (toImm ty) ] (toUni ty)


#
# Unops
#
unaryPlus as Op.Unop =
    {
    , symbol = "0 +"
    , type = tyFn [ number ] number
    , usr = usr "unaryPlus"
    }


unaryMinus as Op.Unop =
    {
    , symbol = "0 -"
    , type = tyFn [ number ] number
    , usr = usr "unaryMinus"
    }


#
# Binops
#
binops as [ Op.Binop ] =
    [
    , and_
    , or_
    , textConcat
    , listCons
    , tuple
    #
    , add
    , subtract
    , multiply
    , divide
    #
    , mutableAssign
    , mutableAdd
    , mutableSubtract
    #
    , equal
    , notEqual
    , lesserThan
    , greaterThan
    , lesserOrEqualThan
    , greaterOrEqualThan
    #
    , sendRight
    , sendLeft
    ]


binopsBySymbol as Dict Text Op.Binop =
    List.for Dict.empty binops (fn bop, d: Dict.insert bop.symbol bop d)


#
# Core types ops
#
and_ as Op.Binop =
    {
    , associativity = Op.'right
    , nonFn = []
    , precedence = Op.precedence_logical
    , symbol = "and"
    , type = typeBinopUnique boolType
    , usr = usr "and_"
    }


or_ as Op.Binop =
    {
    , associativity = Op.'right
    , nonFn = []
    , precedence = Op.precedence_logical
    , symbol = "or"
    , type = typeBinopUnique boolType
    , usr = usr "or_"
    }


textConcat as Op.Binop =
    {
    , associativity = Op.'right
    , nonFn = []
    , precedence = Op.precedence_addittive
    , symbol = ".."
    , type = typeBinopUnique text
    , usr = usr "concat"
    }


listCons as Op.Binop =
    item =
        tyVar "item"

    {
    , associativity = Op.'right
    , nonFn = []
    , precedence = Op.precedence_cons
    , symbol = "::"
    , type = typeBinop item (listType item) (listType item)
    , usr = usr "stack"
    }


tuple as Op.Binop =
    {
    , associativity = Op.'nonAssociative
    , nonFn = []
    , precedence = Op.precedence_tuple
    , symbol = "&"
    , type =
        Dict.empty
        >> Dict.insert "first" (tyVar "a") __
        >> Dict.insert "second" (tyVar "b") __
        >> CA.'typeRecord Pos.'n __
        >> typeBinop (tyVar "a") (tyVar "b") __
    , usr = usr "<& is just sugar>"
    }


#
# Arithmetic ops
#
add as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = []
    , precedence = Op.precedence_addittive
    , symbol = "+"
    , type = typeBinopUnique number
    , usr = usr "add"
    }


subtract as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = []
    , precedence = Op.precedence_addittive
    , symbol = "-"
    , type = typeBinopUnique number
    , usr = usr "subtract"
    }


multiply as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = []
    , precedence = Op.precedence_multiplicative
    , symbol = "*"
    , type = typeBinopUnique number
    , usr = usr "multiply"
    }


divide as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = []
    , precedence = Op.precedence_multiplicative
    , symbol = "/"
    , type = typeBinopUnique number
    , usr = usr "divide"
    }


#
# Mut ops
#
mutableAssign as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = []
    , precedence = Op.precedence_mutop
    , symbol = ":="
    , type = CA.'typeFn Pos.'n [ CA.'parRe (tyVar "a"), CA.'parSp { raw = tyVar "a", uni = 'uni } ] { raw = noneType, uni = 'imm }
    , usr = usr "mutableAssign"
    }


mutableAdd as Op.Binop =
    {
    , associativity = Op.'nonAssociative
    , nonFn = []
    , precedence = Op.precedence_mutop
    , symbol = "+="
    , type = CA.'typeFn Pos.'n [ CA.'parRe number, CA.'parSp { raw = number, uni = 'imm } ] { raw = noneType, uni = 'imm }
    , usr = usr "mutableAdd"
    }


mutableSubtract as Op.Binop =
    {
    , associativity = Op.'nonAssociative
    , nonFn = []
    , precedence = Op.precedence_mutop
    , symbol = "-="
    , type = CA.'typeFn Pos.'n [ CA.'parRe number, CA.'parSp { raw = number, uni = 'imm } ] { raw = noneType, uni = 'imm }
    , usr = usr "mutableSubtract"
    }


#
# Comparison
#
equal as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = [ "a" ]
    , precedence = Op.precedence_comparison
    , symbol = "=="
    , type = typeBinop (tyVar "a") (tyVar "a") boolType
    , usr = usr "equal"
    }


notEqual as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = [ "a" ]
    , precedence = Op.precedence_comparison
    , symbol = "/="
    , type = typeBinop (tyVar "a") (tyVar "a") boolType
    , usr = usr "notEqual"
    }


lesserThan as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = [ "a" ]
    , precedence = Op.precedence_comparison
    , symbol = "<"
    , type = typeBinop (tyVar "a") (tyVar "a") boolType
    , usr = usr "lesserThan"
    }


greaterThan as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = [ "a" ]
    , precedence = Op.precedence_comparison
    , symbol = ">"
    , type = typeBinop (tyVar "a") (tyVar "a") boolType
    , usr = usr "greaterThan"
    }


lesserOrEqualThan as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = [ "a" ]
    , precedence = Op.precedence_comparison
    , symbol = "<="
    , type = typeBinop (tyVar "a") (tyVar "a") boolType
    , usr = usr "lesserOrEqualThan"
    }


greaterOrEqualThan as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = [ "a" ]
    , precedence = Op.precedence_comparison
    , symbol = ">="
    , type = typeBinop (tyVar "a") (tyVar "a") boolType
    , usr = usr "greaterOrEqualThan"
    }


#
# Send
#
sendRight as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = []
    , precedence = Op.precedence_pipe
    , symbol = ">>"
    , type = typeBinop (tyVar "a") (tyFn [ tyVar "a" ] (tyVar "b")) (tyVar "b")
    , usr = usr "sendRight"
    }


sendLeft as Op.Binop =
    {
    , associativity = Op.'right
    , nonFn = []
    , precedence = Op.precedence_pipe
    , symbol = "<<"
    , type = typeBinop (tyFn [ tyVar "a" ] (tyVar "b")) (tyVar "a") (tyVar "b")
    , usr = usr "sendLeft"
    }


#
# Module
#
insert as fn USR, CA.RawType, [ Name ], Dict Name CA.ValueDef: Dict Name CA.ValueDef =
    fn 'USR _ name, raw, nonFnAsList, dict:
    nonFn =
        Set.fromList nonFnAsList

    tyvars as Dict Name { nonFn as Maybe Pos } =
        raw
        >> CA.typeTyvars __
        >> Dict.map (fn n, pos: { nonFn = if Set.member n nonFn then 'just Pos.'n else 'nothing }) __

    {
    , directDeps = Compiler/MakeCanonical.typeDeps raw Dict.empty
    , maybeAnnotation =
        'just
            {
            , raw
            , tyvars
            , univars = Dict.empty
            }
    , maybeBody = 'nothing
    , name
    , namePos = Pos.'n
    }
    >> Dict.insert name __ dict


coreModule as CA.Module =
    variantTypeDefs as Dict Name CA.VariantTypeDef =
        [
        , textDef
        , numberDef
        , noneTypeDef
        , boolDef
        , listDef
        ]
        >> List.for Dict.empty __ fn def, dict:
            'USR _ name =
                def.usr

            Dict.insert name def dict

    constructorDefs as Dict Name CA.ConstructorDef =
        [
        , noneConsDef
        , trueDef
        , falseDef
        , nilDef
        , consDef
        ]
        >> List.for Dict.empty __ (fn def, dict: Dict.insert def.name def dict)

    valueDefs as Dict Name CA.ValueDef =
        Dict.empty
        >> List.for __ [ unaryPlus, unaryMinus ] (fn unop, dict: insert unop.usr unop.type [] dict)
        >> List.for __ binops (fn binop, dict: insert binop.usr binop.type binop.nonFn dict)

    {
    , aliasDefs = Dict.empty
    , asText = "<core module>"
    , constructorDefs
    , fsPath = "<core module>"
    , umr
    , valueDefs
    , variantTypeDefs
    }
