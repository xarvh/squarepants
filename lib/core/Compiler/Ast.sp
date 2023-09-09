#
# The name of a variable, type, attribute or "visibleAs" module name
#
Name =
    Text


UnivarId =
    Int


#
# A reference to a defined variable
#
var Ref =
    , # This is for stuff defined inside the current function/block
      'refLocal Name
    , # This is for stuff defined at root level
      'refGlobal USR
    , # This is for generated function argument placeholders
      'refPlaceholder Int


var Uniqueness =
    , 'uni
    , 'imm
    , 'depends UnivarId


toImm as fn raw: { raw as raw, uni as Uniqueness } =
    fn raw:
    { raw, uni = 'imm }


toUni as fn raw: { raw as raw, uni as Uniqueness } =
    fn raw:
    { raw, uni = 'uni }
