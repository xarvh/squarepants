#
# Introspection & dynamic loading
#
# TODO this makes core dependent on an external lib
# Might need to move all of Compiler into core
#

var Value =
    , 'value Value


Self =
    {
    , expression as EA.Expression
    , nonFn as Array TA.TyvarId
    , raw as TA.RawType
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
LoadPars =
    {
    , constructors as [ USR & TA.FullType ]
    , defs as [ EA.GlobalDefinition ]
    , entryUsr as USR
    , externalValues as Array { self as Self.Self, usr as USR }
    , type as TA.RawType
    }


load as fn LoadPars, fn specific: general: Result TA.RawType general =
    fn a, b:
    todo "native: load"
