#
# Introspection & dynamic loading
#
# TODO this makes core dependent on an external lib
# Might need to move all of Compiler into core
#

union Value =
    Value Value


alias Self =
    {
    , expr as EA.Expression
    , raw as TA.RawType
    , nonFn as Array TA.TyvarId
    , value as Value
    }


introspect as fn a: Self =
    fn a:
    todo "native: introspect"


internalRepresentation as fn a: Text with a NonFunction =
    fn a:
    todo "native: internalRepresentation"


# If this type was in Compiler/Compiler we'd run the risk of changing it
# forgetting to also change the native implementation of `load`, which is
# not protected by the type check.
alias LoadPars =
    {
    , entryName as Text
    , type as TA.RawType
    , state as Compiler/MakeEmittable.State
    , defs as [EA.GlobalDefinition]
    , constructors as [USR & TA.FullType]
    , externalValues as Array { name as Text, exposed as Self.Self }
    }

load as fn LoadPars, (fn specific: general): Result TA.RawType general =
    fn a, b:
    todo "native: load"

