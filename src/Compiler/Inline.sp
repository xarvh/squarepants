

# TODO this will need to tell us whether a function should be deeply or shallowly inlined
alias Env = {}

union Outcome =
    , Inline.Circular [Name]
    , Inline.Inlined TA.Expression
    , Inline.NotInlined


inline as fn Env, Dict Name TA.Expression, TA.Expression, [TA.Argument]: Outcome =
    fn env, locals, ref, args:

    todo "inline"

