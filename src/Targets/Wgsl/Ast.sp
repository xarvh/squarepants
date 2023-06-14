
alias Name =
    Text


alias AttrName =
    Text


union Expression =
    , LiteralNumber Number
    , Variable Ref
    , Call Expression [Argument]
    , Fn [Bool & Maybe Name] [Statement]

    , PatternMatchConditions [Expression]
    , CompareWithLiteralText Text Expression
    , CompareWithLiteralNumber Number Expression
    , IsConstructor Name Expression

    , Constructor USR
    , ConstructorAccess Int Expression

    , LiteralRecord (Maybe Expression) [AttrName & Expression]
    , RecordAccess AttrName Expression


union Argument =
    , ArgumentRecycle TA.RawType [Name] Name
    , ArgumentSpend TA.FullType Expression


union Statement =
    , VarDefinition
          {
          , name as Name
          , type as TA.FullType
          , value as Expression
          }
    , FnDefinition
          {
          , name as Name
          , type as TA.FullType
          , args as [Bool & Maybe Name]
          , body as [Statement]
          }
    , IfStatement Expression [Statement] [Statement]
    , Execution Expression
    , Assignment Name Expression
    , Return Expression
    , MissingPattern Text Expression


alias GlobalDefinition =
    {
    , usr as USR
    , stats as [Statement]

    # We need these to be able to put defs in the right order
    , deps as Set USR
    }

