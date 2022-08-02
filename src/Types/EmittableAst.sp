#
# Right now, we emit JS so this maps well to JS.
#
# Once we will target lower level languages (Wasm, Spir-V, LLVM?) we'll
# modify this to better describe those.
#

alias Name =
    Text


alias AttrName =
    Text


union Expression =
    , LiteralText Text
    , LiteralNumber Number
    , Variable Name
    , Call Expression [Argument]
    , Fn [Bool & Maybe Name] Expression
    , Conditional Expression Expression Expression
    , And [Expression]
    , ShallowEqual Expression Expression
    , LetIn
        {
        , maybeName as Maybe Name
        , type as TA.FullType
        , letExpression as Expression
        , inExpression as Expression
        }
    , LiteralArray [Expression]
    , ArrayAccess Int Expression
    , Constructor Name
    , ConstructorAccess Int Expression
    , IsConstructor Name Expression
    , LiteralRecord (Maybe Expression) [AttrName & Expression]
    , RecordAccess AttrName Expression

    # TODO Replace `pos` with the directly calculated location, so that errorEnv is not necessary any more?
    , MissingPattern Pos Expression


union Argument =
    , ArgumentRecycle [Name] Name
    , ArgumentSpend Expression


alias GlobalDefinition =
    {
    , name as Name
    , expr as Expression

    # We need these to be able to put defs in the right order
    , deps as Set Name
    }

