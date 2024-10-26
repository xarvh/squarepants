#
# Introspection & dynamic loading
#
LoadPars =
    {
    , constructors as [ USR & TA.RawType ]
    , defs as [ EA.GlobalDefinition ]
    , entryUsr as EA.TranslatedUsr
    , type as TA.RawType
    }


load as fn LoadPars, fn specific: general: Result TA.RawType general =
    this_is_sp_native

#    # This is here to add the dependency
#    BuildInfo.compile
#
#    load_internal
#
#
#load_internal as fn LoadPars, fn specific: general: Result TA.RawType general =


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
    , 'value CA.ValueDef
    , 'openVarType CA.VariantTypeDef
    , 'openAliasType CA.AliasDef
    , 'opaqueType CA.VariantTypeDef


Self =
    {
    , def as Def
    , usr as USR
    }


toCaModules as fn [ Self ]: Dict UMR CA.Module =
    fn selfs:
    List.for Dict.empty selfs fn modulesByUmr, self:
        'USR umr name =
            self.usr

        mod0 =
            try Dict.get umr modulesByUmr as
                'just m: m
                'nothing: CA.initModule "<internal>" umr ""

        mod1 =
            try self.def as

                'value def:
                    maybeBody =
                        'nothing

                    directDeps =
                        # TODO split between "annotation dependencies" and "definition dependencies"?
                        Dict.filter (__ == Meta.'typeDependency) def.directDeps

                    { mod0 with valueDefs = Dict.insert .valueDefs name { def with directDeps, maybeBody } }

                'openVarType def:
                    { mod0 with
                    , constructorDefs = todo "constructorDefs"
                    , variantTypeDefs = Dict.insert .variantTypeDefs name def
                    }

                'openAliasType def:
                    { mod0 with aliasDefs = Dict.insert .aliasDefs name defaliasDefs }

                'opaqueType def:
                    { mod0 with variantTypeDefs = Dict.insert .variantTypeDefs name { def with constructors = Dict.empty } }

        Dict.insert modulesByUmr umr mod1
