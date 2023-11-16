#
# Introspection & dynamic loading
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
    this_is_sp_native


internalRepresentation as fn a: Text with a NonFunction =
    this_is_sp_native


# If this type was in Compiler/Compiler we'd run the risk of changing it
# forgetting to also change the native implementation of `load`, which is
# not protected by the type check.
LoadPars =
    {
    , constructors as [ USR & TA.RawType ]
    , defs as [ EA.GlobalDefinition ]
    , entryUsr as USR
    , type as TA.RawType
    }


load as fn LoadPars, fn specific: general: Result TA.RawType general =
    this_is_sp_native
