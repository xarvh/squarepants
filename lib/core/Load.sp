#
# Dynamic loading
#

union Value =
    Value Value


alias Exposed =
    {
    , raw as TA.RawType
    , nonFn as Array TA.TyvarId
    , value as Value
    }

# This funciton exists for 2 reasons:
#   1. I want ExposedValue to be able to contain any type of value, without any tyvar on ExposedValue
#   2. Collect the inferred type
expose as fn a: Exposed =
    fn a:
    todo "native: expose"


#getExposedType as fn ExposedValue: { raw as TA.RawType, nonFn as [TA.TyvarId] } =
#    fn e:
#    todo "native: getExposedType"


internalRepresentation as fn a: Text with a NonFunction =
    fn a:
    todo "native: internalRepresentation"


#
# TODO this makes core dependent on an external lib
# Might need to move all of Compiler into core
#
dynamicLoad as fn Compiler/Compiler.CompileModulesOut, (fn specific: general): Result TA.RawType general =
    todo "platform does not override dynamicLoad"

