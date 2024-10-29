#
# TODO Rename to LoadableAST?
# TODO Move in Self?
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


#
# TODO removing this will cause an error elsewhere in the code
# rather than in the RawType definition below.
#
TyvarId =
    Int


# TODO once we have monomorphization, this is not needed any more
UnivarId =
    Int


var Uniqueness =
    , 'uni
    , 'imm
    , 'depends UnivarId


toImm as fn raw: { raw as raw, uni as Uniqueness } =
    fn raw:
    { raw, uni = 'imm }


toUni as fn raw: { raw as raw, uni as Uniqueness } =
    fn raw:
    { raw, uni = 'uni }


var RawType =
    , 'typeExact TranslatedUsr [ RawType ]
    , 'typeFn [ ParType ] FullType
    , 'typeVar TyvarId
    , 'typeRecord (Maybe TyvarId) (Dict Name RawType)


var ParType =
    , 'parRe RawType
    , 'parSp FullType


FullType =
    {
    , raw as RawType
    , uni as Uniqueness
    }


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
    , 'isLiteralText Text Expression
    , 'isLiteralNumber Number Expression
    , 'letIn
          {
          , inExpression as Expression
          , letExpression as Expression
          , maybeName as Maybe Name
          , type as FullType
          }
    , 'constructor TranslatedUsr
    , 'constructorAccess Int Expression
    , 'isConstructor TranslatedUsr Expression
    , 'literalRecord (Maybe Expression) [ AttrName & Expression ]
    , 'recordAccess AttrName Expression
    , 'missingPattern Text Expression
    , 'introspect Self.Self


var Argument =
    , 'argumentRecycle RawType [ Name ] Name
    , 'argumentSpend FullType Expression


GlobalDefinition =
    {
    # We need deps to be able to put defs in the right order
    # TODO no we don't need them any more. =|
#    , deps as Set TranslatedUsr
    , expr as Expression
#    , freeTyvars as Dict TyvarId Tyvar
#    , freeUnivars as Dict UnivarId Univar
    , type as RawType
    , usr as TranslatedUsr
    }
