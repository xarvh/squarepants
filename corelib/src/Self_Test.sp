valueTest as fn Text, fn None: a, Test.CodeExpectation a: Test =
    Test.valueTest Debug.toHuman __ __ __


tests as Test =
    Test.'group
        "SKIP (needs an overhaul) Self"
        [
        , base
        ]


var IntegerOnly =
    , 'integer Int


load as fn a: Result EA.RawType IntegerOnly =
    fn _:

    translatedUsr =
        [ "u", "5", "DynamicLoad", "aNumberValue" ]

    numberSelf =
        sp_introspect_type Number

    numberType as EA.RawType =
        todo "EA.'typeExact numberSelf.usr []"

    def as EA.GlobalDefinition =
        {
#        , deps = Dict.empty
        , expr = EA.'literalNumber 42
#        , freeTyvars = Dict.empty
#        , freeUnivars = Dict.empty
        , type = numberType
        , usr = translatedUsr
        }

    pars as Self.LoadPars =
        {
        , constructors = []
        , defs = [ def ]
        , entryUsr = translatedUsr
        , type = numberType
        }

    Self.load pars 'integer


base as Test =
    valueTest
        """
        Can actually sort stuff
        """
        load
        (Test.isOkAndEqualTo ('ok ('integer 42)))
