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


var Expression =
    , 'literalText Text
    , 'literalNumber Number
    , 'variable Ref
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
    , 'constructor USR
    , 'constructorAccess Int Expression
    , 'isConstructor USR Expression
    , 'literalRecord (Maybe Expression) [ AttrName & Expression ]
    , 'recordAccess AttrName Expression
    , 'missingPattern Text Expression


var Argument =
    , 'argumentRecycle TA.RawType [ Name ] Name
    , 'argumentSpend TA.FullType Expression


GlobalDefinition =
    {
    # We need these to be able to put defs in the right order
    , deps as Set USR
    , expr as Expression
    , usr as USR
    }
