[#

* No arbitrary let..ins: variables can only be declared inside blocks.
  (SP doesn't really allow arbitrary let..ins either...)

* No inline functions

* No pattern matching

#]


alias Name =
    Text


alias AttrName =
    Text


union Expression =
    , LiteralText Text
    , LiteralNumber Number
    , Variable Ref
    , Call Expression [Argument]

    , IfExpression Expression Expression Expression

    , PatternMatchConditions [Expression]
    , CompareWithLiteralText Text Expression
    , CompareWithLiteralNumber Number Expression
    , IsConstructor Name Expression

    , Constructor USR
    , ConstructorAccess Int Expression

    , LiteralRecord (Maybe Expression) [AttrName & Expression]
    , RecordAccess AttrName Expression

    , MissingPattern Text Expression


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
    , Evaluation Expression
    , Return Expression


alias GlobalDefinition =
    {
    , usr as USR
    , stats as [Statement]

    # We need these to be able to put defs in the right order
    , deps as Set USR
    }

