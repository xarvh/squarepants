# A reference to a defined variable
union Ref =
    # This is for stuff defined inside the current function/block
    , RefLocal Name
    # This is for stuff defined at root level
    , RefGlobal USR

union UniqueOrImmutable =
    , Uni
    , Imm

union RecycleOrSpend =
    , Recycle
    , Spend
