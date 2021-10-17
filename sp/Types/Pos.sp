#
# The position of a piece of code
#
union Pos =
    # actual position: module reference, start, end
    , P Text Int Int
    # stripped
    , S
    # defined natively, usually in Core or Prelude
    , N
    # defined as test
    , T
    # inferred
    , I Int

    #  TODO the following ones need to be removed

    # error todo
    , E
    # Formattable to canonical todo
    , F
    # ScopeCheck HACK
    , G
    # Union
    , U
