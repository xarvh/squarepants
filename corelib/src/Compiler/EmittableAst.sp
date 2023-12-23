#
# Right now, we emit JS so this maps well to JS.
#
# Once we will target lower level languages (Wasm, Spir-V, LLVM?) we'll
# modify this to better describe those.
#

Name =
    Text


AttrName =
    Text


TranslatedUsr =
    [ Text ]


var Expression =
    , 'literalText Text
    , 'literalNumber Number
    , 'localVariable Name
    , 'globalVariable TranslatedUsr
    , 'placeholderVariable Int
    , 'call Expression [ Argument ]
    , 'fn [ Bool & Maybe Name ] Expression
    , 'conditional Expression Expression Expression
    , 'and [ Expression ]
    , 'shallowEqual Expression Expression
    , 'letIn
          {
          , inExpression as Expression
          , letExpression as Expression
          , maybeName as Maybe Name
          , type as TA.FullType
          }
    , 'literalArray [ Expression ]
    , 'arrayAccess Int Expression
    , 'constructor TranslatedUsr
    , 'constructorAccess Int Expression
    , 'isConstructor USR Expression
    , 'literalRecord (Maybe Expression) [ AttrName & Expression ]
    , 'recordAccess AttrName Expression
    , 'missingPattern Text Expression
    , 'introspect Self.Self


var Argument =
    , 'argumentRecycle TA.RawType [ Name ] Name
    , 'argumentSpend TA.FullType Expression


GlobalDefinition =
    {
    # We need deps to be able to put defs in the right order
    # TODO no we don't need them any more. =|
    , deps as CA.Deps
    , expr as Expression
    , freeTyvars as Dict TA.TyvarId TA.Tyvar
    , freeUnivars as Dict UnivarId TA.Univar
    , type as TA.RawType
    , usr as TranslatedUsr
    }


#
# Not sure where to put these, but because I need a predictable ImportPath for them, they should stay in Core.
# At least until I find a more reliable way to reference them from within the code itself.
#

TranslationState =
    {
    , importsAndSourceDirCount as Int
    , importsAndSourceDirToId as Hash Text Text
    }


initTranslationState as TranslationState =
    {
    , importsAndSourceDirCount = 0
    , importsAndSourceDirToId = Hash.fromList []
    }


translateRoot as fn Meta.RootDirectory: Text =
    try __ as
        Meta.'core: "c"
        Meta.'user: "u"
        Meta.'installed: "i"


translateName as fn Name: Text =
    fn name:
    if Text.startsWith "'" name then
        head =
            Text.slice 1 2 name

        rest =
            Text.slice 2 9999 name

        Text.toUpper head .. rest
    else
        name


translateUsr as fn Dict Text Int, USR: TranslatedUsr =
    fn sourceDirectoryKeyToId, usr:
    'USR ('UMR (Meta.'importsPath root importsDir) sourceDir modulePath) name =
        usr

    key =
        Meta.sourceDirectoryKey usr

    id =
        try Dict.get key sourceDirectoryKeyToId as
            'just i: i
            'nothing: todo << "compiler bug: translateUsr@" .. key

    List.concat [ [ translateRoot root .. Text.fromNumber id ], Text.split "/" modulePath, [ translateName name ] ]
