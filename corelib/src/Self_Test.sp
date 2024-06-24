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


load as fn a: Result TA.RawType IntegerOnly =
    fn _:
    usr as USR =
        'USR ('UMR Meta.'user 5 "DynamicLoad") "aNumberValue"

    translatedUsr =
        EA.translateUsr usr 0

    numberSelf =
        sp_introspect_type Number

    numberType as TA.RawType =
        TA.'typeExact Pos.'t numberSelf.usr []

    def as EA.GlobalDefinition =
        {
        , deps = Dict.empty
        , expr = EA.'literalNumber 42
        , freeTyvars = Dict.empty
        , freeUnivars = Dict.empty
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
