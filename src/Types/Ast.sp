#
# The name of a variable, type, attribute or "visibleAs" module name
#
alias Name =
    Text


alias UnivarId =
    Int


#
# A reference to a defined variable
#
union Ref =
    # This is for stuff defined inside the current function/block
    , RefLocal Name
    # This is for stuff defined at root level
    , RefGlobal USR


union Uniqueness =
    , Uni
    , Imm
    , Depends UnivarId


toImm as raw: { raw as raw, uni as Uniqueness } =
    raw:
    { raw, uni = Imm }


toUni as raw: { raw as raw, uni as Uniqueness } =
    raw:
    { raw, uni = Uni }

