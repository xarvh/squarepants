



reduceExpression as fn Env, EA.Expression: Res EA.Expression =
    fn env, expr:

    try expr as
        , LiteralText Text
        , LiteralNumber Number
        , Variable Ref
        , Fn [Bool & Maybe Name] Expression
        , LiteralArray [Expression]
        , Constructor USR
        , LiteralRecord (Maybe Expression) [AttrName & Expression]
        , MissingPattern Text Expression

        , Call Expression [Argument]
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
        , ArrayAccess Int Expression
        , ConstructorAccess Int Expression
        , IsConstructor Name Expression
        , RecordAccess AttrName Expression
