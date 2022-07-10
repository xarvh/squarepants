

union State =
    , Available_CollectingArgs
    , Available_TraversingBody
    , NotAvailable # TODO { pos as Pos, reason as Text }


alias Env = {
    , currentName as Name
    , args as [Name]
    , state as State
    }



fromGlobalDef as EA.GlobalDefinition: EA.GlobalDefinition =
    gdef:

    env as Env = {
        , currentName = gdef.name
        , args = []
        , state = NotAvailable
        }


fromExpression as Env: EA.Expression: EA.Expression =
    env: expression:

    try expression as
        EA.Lambda (Maybe Name & Mutability) Expression

        EA.Call Expression (Expression & Mutability)

        EA.LetIn {
    #        , maybeName as Maybe Name
    #        # type as Type
    #        , mutability as Mutability
    #        , letExpression as Expression
    #        , inExpression as Expression
            }

        EA.Conditional Expression Expression Expression



        EA.LiteralArray [Expression]


        EA.LiteralText Text:
          notAvailable

        EA.LiteralNumber Number
          notAvailable

        EA.Variable Name [Name]



        EA.And [Expression]
        EA.ShallowEqual Expression Expression
        EA.ArrayAccess Int Expression
        EA.Constructor Name
        EA.ConstructorAccess Int Expression
        EA.IsConstructor Name Expression
        EA.LiteralRecord (Maybe Expression) [AttrName & Expression]
        EA.RecordAccess AttrName Expression
        EA.MissingPattern Pos Expression




