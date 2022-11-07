
alias Int =
    Number


coreUsr as Text: Meta.UniqueSymbolReference =
    Meta.USR (Meta.UMR Meta.Core "Core")


listUsr as Text: Meta.UniqueSymbolReference =
    Meta.USR (Meta.UMR Meta.Core "List")


textUsr as Text: Meta.UniqueSymbolReference =
    Meta.USR (Meta.UMR Meta.Core "Text")


numberUsr as Text: Meta.UniqueSymbolReference =
    Meta.USR (Meta.UMR Meta.Core "Number")


debugUsr as Text: Meta.UniqueSymbolReference =
    Meta.USR (Meta.UMR Meta.Core "Debug")


tupleUsr as Text: Meta.UniqueSymbolReference =
    Meta.USR (Meta.UMR Meta.Core "Tuple")


#
# Helpers
#

tyVar as Name: CA.CanonicalType =
    name:
    CA.TypeAnnotationVariable Pos.N name


tyFun as CA.CanonicalType: CA.CanonicalType: CA.CanonicalType =
    from: to:
    CA.TypeFunction Pos.N from LambdaNormal to


typeUnopUniform as CA.CanonicalType: CA.CanonicalType =
    type:
    tyFun type type


typeBinop as CA.CanonicalType: CA.CanonicalType: CA.CanonicalType: CA.CanonicalType =
    left: right: return:
    tyFun
        right
        (tyFun
            left
            return
        )


typeBinopUniform as CA.CanonicalType: CA.CanonicalType =
    ty:
    typeBinop ty ty ty



#
# Unops
#
unaryPlus as Op.Unop = {
    , usr = numberUsr "unaryPlus"
    , symbol = "0 +"
    , type = typeUnopUniform CoreTypes.number
    }


unaryMinus as Op.Unop = {
    , usr = numberUsr "unaryMinus"
    , symbol = "0 -"
    , type = typeUnopUniform CoreTypes.number
    }


#
# Binops
#
binops as [Op.Binop] = [
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
#    , mutableAssign
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
    Dict.empty >> List.for binops (bop: Dict.insert bop.symbol bop)


#
# Core types ops
#
and_ as Op.Binop = {
    , usr = coreUsr "and_"
    , symbol = "and"
    , precedence = Op.Logical
    , associativity = Op.Right
    , type = typeBinopUniform CoreTypes.bool
    , nonFn = []
    }


or_ as Op.Binop = {
    , usr = coreUsr "or_"
    , symbol = "or"
    , precedence = Op.Logical
    , associativity = Op.Right
    , type = typeBinopUniform CoreTypes.bool
    , nonFn = []
    }


textConcat as Op.Binop = {
    , usr = textUsr "concat"
    , symbol = ".."
    , precedence = Op.Addittive
    , associativity = Op.Right
    , type = typeBinopUniform CoreTypes.text
    , nonFn = []
    }


listCons as Op.Binop =
    item =
        tyVar "item"

    {
    , usr = listUsr "stack"
    , symbol = "::"
    , precedence = Op.Cons
    , associativity = Op.Right
    , type = typeBinop item (CoreTypes.list item) (CoreTypes.list item)
    , nonFn = []
    }


tuple as Op.Binop = {
    # TODO Add a flag to ensure that these syntactic sugar ops are never added to the module
    , usr = tupleUsr "pair_$$$$"
    , symbol = "&"
    , precedence = Op.Tuple
    , associativity = Op.NonAssociative
    , type =
        Dict.empty
            >> Dict.insert "first" (tyVar "a")
            >> Dict.insert "second" (tyVar "b")
            >> CA.TypeRecord Pos.N
            >> typeBinop (tyVar "a") (tyVar "b")
    , nonFn = []
    }



#
# Arithmetic ops
#
add as Op.Binop = {
    , usr = numberUsr "add"
    , symbol = "+"
    , precedence = Op.Addittive
    , associativity = Op.Left
    , type = typeBinopUniform CoreTypes.number
    , nonFn = []
    }


subtract as Op.Binop = {
    , usr = numberUsr "subtract"
    , symbol = "-"
    , precedence = Op.Addittive
    , associativity = Op.Left
    , type = typeBinopUniform CoreTypes.number
    , nonFn = []
    }


multiply as Op.Binop = {
    , usr = numberUsr "multiply"
    , symbol = "*"
    , precedence = Op.Multiplicative
    , associativity = Op.Left
    , type = typeBinopUniform CoreTypes.number
    , nonFn = []
    }


divide as Op.Binop = {
    , usr = numberUsr "divide"
    , symbol = "/"
    , precedence = Op.Multiplicative
    , associativity = Op.Left
    , type = typeBinopUniform CoreTypes.number
    , nonFn = []
    }


#
# Mut ops
#
#mutableAssign as Op.Binop = {
#    , usr = coreUsr "mutableAssign"
#    , symbol = ":="
#    , precedence = Op.Mutop
#    , associativity = Op.Left
#    , type = typeBinop True (tyVar "a") (tyVar "a") CoreTypes.none
#    , nonFn = []
#    }


mutableAdd as Op.Binop = {
    , usr = numberUsr "mutableAdd"
    , symbol = "+="
    , precedence = Op.Mutop
    , associativity = Op.NonAssociative
    , type = typeBinop (CA.TypeUnique Pos.N CoreTypes.number) CoreTypes.number CoreTypes.none
    , nonFn = []
    }


mutableSubtract as Op.Binop = {
    , usr = numberUsr "mutableSubtract"
    , symbol = "-="
    , precedence = Op.Mutop
    , associativity = Op.NonAssociative
    , type = typeBinop (CA.TypeUnique Pos.N CoreTypes.number) CoreTypes.number CoreTypes.none
    , nonFn = []
    }


#
# Comparison
#
equal as Op.Binop = {
    , usr = coreUsr "equal"
    , symbol = "=="
    , precedence = Op.Comparison
    , associativity = Op.Left
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.bool
    , nonFn = [ "a" ]
    }


notEqual as Op.Binop = {
    , usr = coreUsr "notEqual"
    , symbol = "/="
    , precedence = Op.Comparison
    , associativity = Op.Left
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.bool
    , nonFn = [ "a" ]
    }


lesserThan as Op.Binop = {
    , usr = coreUsr "lesserThan"
    , symbol = "<"
    , precedence = Op.Comparison
    , associativity = Op.Left
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.bool
    , nonFn = [ "a" ]
    }


greaterThan as Op.Binop = {
    , usr = coreUsr "greaterThan"
    , symbol = ">"
    , precedence = Op.Comparison
    , associativity = Op.Left
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.bool
    , nonFn = [ "a" ]
    }


lesserOrEqualThan as Op.Binop = {
    , usr = coreUsr "lesserOrEqualThan"
    , symbol = "<="
    , precedence = Op.Comparison
    , associativity = Op.Left
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.bool
    , nonFn = [ "a" ]
    }


greaterOrEqualThan as Op.Binop = {
    , usr = coreUsr "greaterOrEqualThan"
    , symbol = ">="
    , precedence = Op.Comparison
    , associativity = Op.Left
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.bool
    , nonFn = [ "a" ]
    }



#
# Send
#
sendRight as Op.Binop = {
    , usr = coreUsr "sendRight"
    , symbol = ">>"
    , precedence = Op.Pipe
    , associativity = Op.Left
    , type =
        typeBinop
            (tyVar "a")
            (tyFun (tyVar "a") (tyVar "b"))
            (tyVar "b")
    , nonFn = []
    }


sendLeft as Op.Binop = {
    , usr = coreUsr "sendLeft"
    , symbol = "<<"
    , precedence = Op.Pipe
    , associativity = Op.Right
    , type =
        typeBinop
            (tyFun (tyVar "a") (tyVar "b"))
            (tyVar "a")
            (tyVar "b")
    , nonFn = []
    }


#
# Functions
#

alias Function = {
    , usr as Meta.UniqueSymbolReference
    , type as CA.CanonicalType
    , nonFn as [Text]
    }


functions as [Function] = [
    # TODO with the new Platforms system, there is no real reasons for these functions to be here.
    , mut
    , reinit
    , compare
    , debugTodo
    , debugLog
    , debugToHuman
    , debugBenchStart
    , debugBenchStop
    ]


mut as Function = {
    , usr = coreUsr "mut"
    , type = tyFun (tyVar "a") (CA.TypeUnique Pos.N (tyVar "a"))
    , nonFn = [ "a" ]
    }


# TODO remove this one, it's used only by the typecheck tests?
reinit as Function = {
    , usr = coreUsr "reinit"
    , type = tyFun (CA.TypeUnique Pos.N (tyVar "a")) (tyFun (tyVar "a") CoreTypes.none)
    , nonFn = [ "a" ]
    }


compare as Function = {
    , usr = coreUsr "compare"
    , type = tyFun (tyVar "a") (tyFun (tyVar "a") CoreTypes.number)
    , nonFn = [ "a" ]
    }


debugTodo as Function = {
    , usr = debugUsr "todo"
    , type = tyFun CoreTypes.text (tyVar "a")
    , nonFn = []
    }


debugLog as Function = {
    , usr = debugUsr "log"
    , type = tyFun CoreTypes.text (tyFun (tyVar "a") (tyVar "a"))
    , nonFn = []
    }


debugToHuman as Function = {
    , usr = debugUsr "toHuman"
    , type = tyFun (tyVar "a") CoreTypes.text
    , nonFn = []
    }


debugBenchStart as Function = {
    , usr = debugUsr "benchStart"
    , type = tyFun  CoreTypes.none CoreTypes.none
    , nonFn = []
    }


debugBenchStop as Function = {
    , usr = debugUsr "benchStop"
    , type = tyFun CoreTypes.text CoreTypes.none
    , nonFn = []
    }


#
# Modules
#

alias ModuleByUmr =
    Dict Meta.UniqueModuleReference (CA.Module)


insertInModule as Meta.UniqueSymbolReference: CA.CanonicalType: [Name]: ModuleByUmr: ModuleByUmr =
    usr: type: nonFn:

    Meta.USR umr name =
        usr

    tyvars =
        type
        >> CA.typeTyvars
        >> Dict.map tyvarName: pos:
            {
            , allowFunctions = not (List.member tyvarName nonFn) >> Just
            , allowUniques = Just False
            }

    def as CA.ValueDef = {
        , pattern = CA.PatternAny Pos.N { isUnique = False, maybeName = Just name, maybeAnnotation = Just type }
        , native = True
        , body = CA.LiteralText Pos.N name

        , tyvars
        #
        , directTypeDeps = Dict.empty
        , directConsDeps = Dict.empty
        , directValueDeps = Dict.empty
        }

    update as Maybe CA.Module: Maybe CA.Module =
        maybeModule:
        maybeModule
        >> Maybe.withDefault {
            , umr
            , asText = ""
            , aliasDefs = Dict.empty
            , unionDefs = Dict.empty
            , valueDefs = Dict.empty
            }
        >> module: { module with valueDefs = Dict.insert def.pattern def .valueDefs }
        >> Just

    Dict.update umr update


insertUnop as Op.Unop: ModuleByUmr: ModuleByUmr =
    unop:
    insertInModule unop.usr unop.type []


insertBinop as Op.Binop: ModuleByUmr: ModuleByUmr =
    binop:
    insertInModule binop.usr binop.type binop.nonFn


insertFunction as Function: ModuleByUmr: ModuleByUmr =
    function:
    insertInModule function.usr function.type function.nonFn


coreModulesByUmr as Dict Meta.UniqueModuleReference CA.Module =
    Dict.empty
    >> insertUnop unaryPlus
    >> insertUnop unaryMinus
    >> List.for binops insertBinop
    >> List.for functions insertFunction


coreModules as [CA.Module] =
    Dict.values coreModulesByUmr

