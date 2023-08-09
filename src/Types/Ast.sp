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
    # This is for generated function argument placeholders
    , RefPlaceholder Int


union Uniqueness =
    , Uni
    , Imm
    , Depends UnivarId


toImm as fn raw: { raw as raw, uni as Uniqueness } =
    fn raw:
    { raw, uni = Imm }


toUni as fn raw: { raw as raw, uni as Uniqueness } =
    fn raw:
    { raw, uni = Uni }

