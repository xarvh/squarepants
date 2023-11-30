#
# Introspection & dynamic loading
#

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
    List.for Dict.empty selfs fn self, modulesByUmr:
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
                        Dict.filter (fn k, v: v == Meta.'typeDependency) def.directDeps

                    { mod0 with valueDefs = Dict.insert name { def with directDeps, maybeBody } .valueDefs }

                'openVarType def:
                    { mod0 with
                    , constructorDefs = todo "constructorDefs"
                    , variantTypeDefs = Dict.insert name def .variantTypeDefs
                    }

                'openAliasType def:
                    { mod0 with aliasDefs = Dict.insert name def .aliasDefs }

                'opaqueType def:
                    { mod0 with variantTypeDefs = Dict.insert name { def with constructors = Dict.empty } .variantTypeDefs }

        Dict.insert umr mod1 modulesByUmr
