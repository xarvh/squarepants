#
# Introspection & dynamic loading
#
LoadPars =
    {
    , constructors as [ EA.TranslatedUsr & EA.RawType ]
    , defs as [ EA.GlobalDefinition ]
    , entryUsr as EA.TranslatedUsr
    , type as EA.RawType
    }


#
# ************ DANGER ************
#
# If you use this, --> ALL NORMAL GUARANTEES OF THE LANGUAGE ARE OFF <--
# Unless you VERY MUCH know what you're doing, don't use it.
# Let the compiler library wrap it for you instead.
#
# TODO rename it to something more scary?
#
load as fn LoadPars, fn specific: general: Result EA.RawType general =
    this_is_sp_native


internalRepresentation as fn a: Text with a NonFunction =
    this_is_sp_native


[#
    The introspection unops return `Self`

    sp_introspect_type $uppercase
        * Compiler complains if type is not available

    sp_introspect_type_open $uppercase
        * Compiler complains if type is not available
        * Compiler complains if type is not open in the current context

    sp_introspect_value lowercase
        * Compiler complains if value is not available
        * Compiler complains if value is not root
        * Compiler complains if value is not annotated
#]

var Def =
    , 'value #CA.ValueDef
    , 'openVarType #CA.VariantTypeDef
    , 'openAliasType #CA.AliasDef
    , 'opaqueType #CA.VariantTypeDef


Self =
    {
    , def as Def
    , usr as EA.TranslatedUsr
    }
