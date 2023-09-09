Int =
    Number


coreUsr as fn Text: USR =
    'USR ('UMR Meta.'core "Core") __


listUsr as fn Text: USR =
    'USR ('UMR Meta.'core "List") __


textUsr as fn Text: USR =
    'USR ('UMR Meta.'core "Text") __


numberUsr as fn Text: USR =
    'USR ('UMR Meta.'core "Number") __


debugUsr as fn Text: USR =
    'USR ('UMR Meta.'core "Debug") __


tupleUsr as fn Text: USR =
    'USR ('UMR Meta.'core "Tuple") __


#
# Helpers
#

tyVar as fn Name: CA.RawType =
    fn name:
    CA.'typeAnnotationVariable Pos.'n name


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
    , type = tyFn [ CoreTypes.number ] CoreTypes.number
    , usr = numberUsr "unaryPlus"
    }


unaryMinus as Op.Unop =
    {
    , symbol = "0 -"
    , type = tyFn [ CoreTypes.number ] CoreTypes.number
    , usr = numberUsr "unaryMinus"
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
    , type = typeBinopUnique CoreTypes.boolType
    , usr = coreUsr "and_"
    }


or_ as Op.Binop =
    {
    , associativity = Op.'right
    , nonFn = []
    , precedence = Op.precedence_logical
    , symbol = "or"
    , type = typeBinopUnique CoreTypes.boolType
    , usr = coreUsr "or_"
    }


textConcat as Op.Binop =
    {
    , associativity = Op.'right
    , nonFn = []
    , precedence = Op.precedence_addittive
    , symbol = ".."
    , type = typeBinopUnique CoreTypes.text
    , usr = textUsr "concat"
    }


listCons as Op.Binop =
    item =
        tyVar "item"

    {
    , associativity = Op.'right
    , nonFn = []
    , precedence = Op.precedence_cons
    , symbol = "::"
    , type = typeBinop item (CoreTypes.listType item) (CoreTypes.listType item)
    , usr = listUsr "stack"
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
    # TODO Add a flag to ensure that these syntactic sugar ops are never added to the module
    , usr =
        tupleUsr ""
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
    , type = typeBinopUnique CoreTypes.number
    , usr = numberUsr "add"
    }


subtract as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = []
    , precedence = Op.precedence_addittive
    , symbol = "-"
    , type = typeBinopUnique CoreTypes.number
    , usr = numberUsr "subtract"
    }


multiply as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = []
    , precedence = Op.precedence_multiplicative
    , symbol = "*"
    , type = typeBinopUnique CoreTypes.number
    , usr = numberUsr "multiply"
    }


divide as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = []
    , precedence = Op.precedence_multiplicative
    , symbol = "/"
    , type = typeBinopUnique CoreTypes.number
    , usr = numberUsr "divide"
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
    , type = CA.'typeFn Pos.'n [ CA.'parRe (tyVar "a"), CA.'parSp { raw = tyVar "a", uni = 'uni } ] { raw = CoreTypes.noneType, uni = 'imm }
    , usr = coreUsr "mutableAssign"
    }


mutableAdd as Op.Binop =
    {
    , associativity = Op.'nonAssociative
    , nonFn = []
    , precedence = Op.precedence_mutop
    , symbol = "+="
    , type = CA.'typeFn Pos.'n [ CA.'parRe CoreTypes.number, CA.'parSp { raw = CoreTypes.number, uni = 'imm } ] { raw = CoreTypes.noneType, uni = 'imm }
    , usr = numberUsr "mutableAdd"
    }


mutableSubtract as Op.Binop =
    {
    , associativity = Op.'nonAssociative
    , nonFn = []
    , precedence = Op.precedence_mutop
    , symbol = "-="
    , type = CA.'typeFn Pos.'n [ CA.'parRe CoreTypes.number, CA.'parSp { raw = CoreTypes.number, uni = 'imm } ] { raw = CoreTypes.noneType, uni = 'imm }
    , usr = numberUsr "mutableSubtract"
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
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.boolType
    , usr = coreUsr "equal"
    }


notEqual as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = [ "a" ]
    , precedence = Op.precedence_comparison
    , symbol = "/="
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.boolType
    , usr = coreUsr "notEqual"
    }


lesserThan as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = [ "a" ]
    , precedence = Op.precedence_comparison
    , symbol = "<"
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.boolType
    , usr = coreUsr "lesserThan"
    }


greaterThan as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = [ "a" ]
    , precedence = Op.precedence_comparison
    , symbol = ">"
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.boolType
    , usr = coreUsr "greaterThan"
    }


lesserOrEqualThan as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = [ "a" ]
    , precedence = Op.precedence_comparison
    , symbol = "<="
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.boolType
    , usr = coreUsr "lesserOrEqualThan"
    }


greaterOrEqualThan as Op.Binop =
    {
    , associativity = Op.'left
    , nonFn = [ "a" ]
    , precedence = Op.precedence_comparison
    , symbol = ">="
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.boolType
    , usr = coreUsr "greaterOrEqualThan"
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
    , usr = coreUsr "sendRight"
    }


sendLeft as Op.Binop =
    {
    , associativity = Op.'right
    , nonFn = []
    , precedence = Op.precedence_pipe
    , symbol = "<<"
    , type = typeBinop (tyFn [ tyVar "a" ] (tyVar "b")) (tyVar "a") (tyVar "b")
    , usr = coreUsr "sendLeft"
    }


#
# Functions
#

Function =
    {
    , nonFn as [ Text ]
    , type as CA.RawType
    , usr as USR
    }


functions as [ Function ] =
    [
    # TODO with the new Platforms system, there is no real reasons for these functions to be here.
    , compare
    , debugTodo
    , debugLog
    , debugToHuman
    , debugBenchStart
    , debugBenchStop
    ]


compare as Function =
    {
    , nonFn = [ "a" ]
    , type = tyFn [ tyVar "a", tyVar "a" ] CoreTypes.number
    , usr = coreUsr "compare"
    }


debugTodo as Function =
    {
    , nonFn = []
    , type = CA.'typeFn Pos.'n [ CA.'parSp { raw = CoreTypes.text, uni = 'imm } ] { raw = CA.'typeAnnotationVariable Pos.'n "a", uni = 'uni }
    , usr = debugUsr "todo"
    }


debugLog as Function =
    {
    , nonFn = []
    , type = tyFn [ CoreTypes.text, tyVar "a" ] (tyVar "a")
    , usr = debugUsr "log"
    }


debugToHuman as Function =
    {
    , nonFn = []
    , type = tyFn [ tyVar "a" ] CoreTypes.text
    , usr = debugUsr "toHuman"
    }


debugBenchStart as Function =
    {
    , nonFn = []
    , type = tyFn [ CoreTypes.noneType ] CoreTypes.noneType
    , usr = debugUsr "benchStart"
    }


debugBenchStop as Function =
    {
    , nonFn = []
    , type = tyFn [ CoreTypes.text ] CoreTypes.noneType
    , usr = debugUsr "benchStop"
    }


#
# List of all core values, used by TypeCheck
#
CoreValue =
    {
    , nonFn as Dict Name None
    , raw as CA.RawType
    , usr as USR
    }


insertInModule as fn USR, CA.RawType, [ Name ], [ CoreValue ]: [ CoreValue ] =
    fn usr, raw, nonFnAsList, list:
    nonFn =
        Set.fromList nonFnAsList

    [ { nonFn, raw, usr }, list... ]


insertUnop as fn Op.Unop, [ CoreValue ]: [ CoreValue ] =
    fn unop, m:
    insertInModule unop.usr unop.type [] m


insertBinop as fn Op.Binop, [ CoreValue ]: [ CoreValue ] =
    fn binop, m:
    insertInModule binop.usr binop.type binop.nonFn m


insertFunction as fn Function, [ CoreValue ]: [ CoreValue ] =
    fn function, m:
    insertInModule function.usr function.type function.nonFn m


allCoreValues as [ CoreValue ] =
    []
    >> insertUnop unaryPlus __
    >> insertUnop unaryMinus __
    >> List.for __ binops insertBinop
    >> List.for __ functions insertFunction
