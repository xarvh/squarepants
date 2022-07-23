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
    , Variable Name [Name]
    , Call Expression (Expression & Bool)
    , Lambda (Maybe Name & Bool) Expression
    , Conditional Expression Expression Expression
    , And [Expression]
    , ShallowEqual Expression Expression
    , LetIn {
        , maybeName as Maybe Name
        # type as Type
        , isMutable as Bool
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
    , MissingPattern Pos Expression


alias GlobalDefinition = {
    , name as Name
    , expr as Expression
    , deps as Set Name
    }

