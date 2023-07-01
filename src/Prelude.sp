
alias Int =
    Number


coreUsr as fn Text: USR =
    USR (UMR Meta.Core "Core") __


listUsr as fn Text: USR =
    USR (UMR Meta.Core "List") __


textUsr as fn Text: USR =
    USR (UMR Meta.Core "Text") __


numberUsr as fn Text: USR =
    USR (UMR Meta.Core "Number") __


debugUsr as fn Text: USR =
    USR (UMR Meta.Core "Debug") __


tupleUsr as fn Text: USR =
    USR (UMR Meta.Core "Tuple") __


#
# Helpers
#

tyVar as fn Name: CA.RawType =
    fn name:
    CA.TypeAnnotationVariable Pos.N name


tyFn as fn [CA.RawType], CA.RawType: CA.RawType =
    fn pars, to:
    CA.TypeFn Pos.N
        (List.map (fn p: CA.ParSp (toImm p)) pars)
        (toImm to)


typeBinop as fn CA.RawType, CA.RawType, CA.RawType: CA.RawType =
    fn left, right, return:
    tyFn [left, right] return


typeBinopUnique as fn CA.RawType: CA.RawType =
    fn ty:
    CA.TypeFn Pos.N [ CA.ParSp (toImm ty), CA.ParSp (toImm ty)] (toUni ty)


#
# Unops
#
unaryPlus as Op.Unop = {
    , usr = numberUsr "unaryPlus"
    , symbol = "0 +"
    , type = tyFn [CoreTypes.numberType] CoreTypes.numberType
    }


unaryMinus as Op.Unop = {
    , usr = numberUsr "unaryMinus"
    , symbol = "0 -"
    , type = tyFn [CoreTypes.numberType] CoreTypes.numberType
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
and_ as Op.Binop = {
    , usr = coreUsr "and_"
    , symbol = "and"
    , precedence = Op.precedence_logical
    , associativity = Op.Right
    , type = typeBinopUnique CoreTypes.boolType
    , nonFn = []
    }


or_ as Op.Binop = {
    , usr = coreUsr "or_"
    , symbol = "or"
    , precedence = Op.precedence_logical
    , associativity = Op.Right
    , type = typeBinopUnique CoreTypes.boolType
    , nonFn = []
    }


textConcat as Op.Binop = {
    , usr = textUsr "concat"
    , symbol = ".."
    , precedence = Op.precedence_addittive
    , associativity = Op.Right
    , type = typeBinopUnique CoreTypes.textType
    , nonFn = []
    }


listCons as Op.Binop =
    item =
        tyVar "item"

    {
    , usr = listUsr "stack"
    , symbol = "::"
    , precedence = Op.precedence_cons
    , associativity = Op.Right
    , type = typeBinop item (CoreTypes.listType item) (CoreTypes.listType item)
    , nonFn = []
    }


tuple as Op.Binop = {
    # TODO Add a flag to ensure that these syntactic sugar ops are never added to the module
    , usr = tupleUsr ""
    , symbol = "&"
    , precedence = Op.precedence_tuple
    , associativity = Op.NonAssociative
    , type =
        Dict.empty
        >> Dict.insert "first" (tyVar "a") __
        >> Dict.insert "second" (tyVar "b") __
        >> CA.TypeRecord Pos.N __
        >> typeBinop (tyVar "a") (tyVar "b") __
    , nonFn = []
    }



#
# Arithmetic ops
#
add as Op.Binop = {
    , usr = numberUsr "add"
    , symbol = "+"
    , precedence = Op.precedence_addittive
    , associativity = Op.Left
    , type = typeBinopUnique CoreTypes.numberType
    , nonFn = []
    }


subtract as Op.Binop = {
    , usr = numberUsr "subtract"
    , symbol = "-"
    , precedence = Op.precedence_addittive
    , associativity = Op.Left
    , type = typeBinopUnique CoreTypes.numberType
    , nonFn = []
    }


multiply as Op.Binop = {
    , usr = numberUsr "multiply"
    , symbol = "*"
    , precedence = Op.precedence_multiplicative
    , associativity = Op.Left
    , type = typeBinopUnique CoreTypes.numberType
    , nonFn = []
    }


divide as Op.Binop = {
    , usr = numberUsr "divide"
    , symbol = "/"
    , precedence = Op.precedence_multiplicative
    , associativity = Op.Left
    , type = typeBinopUnique CoreTypes.numberType
    , nonFn = []
    }


#
# Mut ops
#
mutableAssign as Op.Binop =
    {
    , usr = coreUsr "mutableAssign"
    , symbol = ":="
    , precedence = Op.precedence_mutop
    , associativity = Op.Left
    , type = CA.TypeFn Pos.N [ CA.ParRe (tyVar "a"), CA.ParSp { uni = Uni, raw = (tyVar "a") }] { uni = Imm, raw = CoreTypes.noneType }
    , nonFn = []
    }


mutableAdd as Op.Binop =
    {
    , usr = numberUsr "mutableAdd"
    , symbol = "+="
    , precedence = Op.precedence_mutop
    , associativity = Op.NonAssociative
    , type = CA.TypeFn Pos.N [ CA.ParRe CoreTypes.numberType, CA.ParSp { uni = Imm, raw = CoreTypes.numberType }] { uni = Imm, raw = CoreTypes.noneType }
    , nonFn = []
    }


mutableSubtract as Op.Binop =
    {
    , usr = numberUsr "mutableSubtract"
    , symbol = "-="
    , precedence = Op.precedence_mutop
    , associativity = Op.NonAssociative
    , type = CA.TypeFn Pos.N [ CA.ParRe CoreTypes.numberType, CA.ParSp { uni = Imm, raw = CoreTypes.numberType }] { uni = Imm, raw = CoreTypes.noneType }
    , nonFn = []
    }


#
# Comparison
#
equal as Op.Binop = {
    , usr = coreUsr "equal"
    , symbol = "=="
    , precedence = Op.precedence_comparison
    , associativity = Op.Left
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.boolType
    , nonFn = [ "a" ]
    }


notEqual as Op.Binop = {
    , usr = coreUsr "notEqual"
    , symbol = "/="
    , precedence = Op.precedence_comparison
    , associativity = Op.Left
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.boolType
    , nonFn = [ "a" ]
    }


lesserThan as Op.Binop = {
    , usr = coreUsr "lesserThan"
    , symbol = "<"
    , precedence = Op.precedence_comparison
    , associativity = Op.Left
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.boolType
    , nonFn = [ "a" ]
    }


greaterThan as Op.Binop = {
    , usr = coreUsr "greaterThan"
    , symbol = ">"
    , precedence = Op.precedence_comparison
    , associativity = Op.Left
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.boolType
    , nonFn = [ "a" ]
    }


lesserOrEqualThan as Op.Binop = {
    , usr = coreUsr "lesserOrEqualThan"
    , symbol = "<="
    , precedence = Op.precedence_comparison
    , associativity = Op.Left
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.boolType
    , nonFn = [ "a" ]
    }


greaterOrEqualThan as Op.Binop = {
    , usr = coreUsr "greaterOrEqualThan"
    , symbol = ">="
    , precedence = Op.precedence_comparison
    , associativity = Op.Left
    , type = typeBinop (tyVar "a") (tyVar "a") CoreTypes.boolType
    , nonFn = [ "a" ]
    }



#
# Send
#
sendRight as Op.Binop = {
    , usr = coreUsr "sendRight"
    , symbol = ">>"
    , precedence = Op.precedence_pipe
    , associativity = Op.Left
    , type =
        typeBinop
            (tyVar "a")
            (tyFn [tyVar "a"] (tyVar "b"))
            (tyVar "b")
    , nonFn = []
    }


sendLeft as Op.Binop = {
    , usr = coreUsr "sendLeft"
    , symbol = "<<"
    , precedence = Op.precedence_pipe
    , associativity = Op.Right
    , type =
        typeBinop
            (tyFn [tyVar "a"] (tyVar "b"))
            (tyVar "a")
            (tyVar "b")
    , nonFn = []
    }


#
# Functions
#

alias Function = {
    , usr as USR
    , type as CA.RawType
    , nonFn as [Text]
    }


functions as [Function] = [
    # TODO with the new Platforms system, there is no real reasons for these functions to be here.
    , compare
    , debugTodo
    , debugLog
    , debugToHuman
    , debugBenchStart
    , debugBenchStop
    ]


compare as Function = {
    , usr = coreUsr "compare"
    , type = tyFn [tyVar "a", (tyVar "a")] CoreTypes.numberType
    , nonFn = [ "a" ]
    }


debugTodo as Function = {
    , usr = debugUsr "todo"
    , type = CA.TypeFn Pos.N [CA.ParSp { uni = Imm, raw = CoreTypes.textType }] { uni = Uni, raw = CA.TypeAnnotationVariable Pos.N "a" }
    , nonFn = []
    }


debugLog as Function = {
    , usr = debugUsr "log"
    , type = tyFn [CoreTypes.textType, (tyVar "a")] (tyVar "a")
    , nonFn = []
    }


debugToHuman as Function = {
    , usr = debugUsr "toHuman"
    , type = tyFn [tyVar "a"] CoreTypes.textType
    , nonFn = []
    }


debugBenchStart as Function = {
    , usr = debugUsr "benchStart"
    , type = tyFn [CoreTypes.noneType] CoreTypes.noneType
    , nonFn = []
    }


debugBenchStop as Function = {
    , usr = debugUsr "benchStop"
    , type = tyFn [CoreTypes.textType] CoreTypes.noneType
    , nonFn = []
    }


#
# List of all core values, used by TypeCheck
#
alias CoreValue =
    {
    , usr as USR
    , raw as CA.RawType
    , nonFn as Dict Name None
    }


insertInModule as fn USR, CA.RawType, [Name], [CoreValue]: [CoreValue] =
  fn usr, raw, nonFnAsList, list:

  nonFn = Set.fromList nonFnAsList

  [{ usr, raw, nonFn }, ...list]


insertUnop as fn Op.Unop, [CoreValue]: [CoreValue] =
    fn unop, m:
    insertInModule unop.usr unop.type [] m


insertBinop as fn Op.Binop, [CoreValue]: [CoreValue] =
    fn binop, m:
    insertInModule binop.usr binop.type binop.nonFn m


insertFunction as fn Function, [CoreValue]: [CoreValue] =
    fn function, m:
    insertInModule function.usr function.type function.nonFn m


allCoreValues as [CoreValue] =
    []
    >> insertUnop unaryPlus __
    >> insertUnop unaryMinus __
    >> List.for __ binops insertBinop
    >> List.for __ functions insertFunction

