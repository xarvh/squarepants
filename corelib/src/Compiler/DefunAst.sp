TyvarId =
    Int


LambdaSetId =
    Int


# This reference can uniquely reference lambdas nested inside a definition
LambdaRef =
    USR & Int


var RawType =
    , 'typeExact USR [ RawType ]
    , 'typeFn (Set LambdaRef) [ ParType ] FullType
    # TODO remove this if we monomorphize
    , 'typeVar TyvarId
    # TODO remove the extension if we monomorphize
    , 'typeRecord (Maybe TyvarId) (Dict Name RawType)


var ParType =
    , 'parRe RawType
    , 'parSp FullType


# Aliases (and opaques!) can't be FullType, they must be RawType!
FullType =
    {
    , raw as RawType
    , uni as Uniqueness
    }


var Expression =
    , 'literalNumber Number
    , 'literalText Text
    , 'localVariable Name
    # This can't point to a function.
    # While, in principle, we could pass function references around, in practice most of the times
    # those functions will need a context.
    , 'globalVariable USR
    , 'constructor USR
    , 'call { ref as LambdaRef, context as Dict Name FullType, args as  [ Argument ] }
    , 'record (Maybe Expression) (Dict Name Expression)
    , 'recordAccess Name Expression
    , 'letIn LocalDef Expression FullType
    , 'if
          {
          , condition as Expression
          , false as Expression
          , true as Expression
          }
    , 'try
          {
          , patternsAndExpressions as [ TA.Pattern & Expression ]
          , value as Expression
          , valueType as FullType
          }
    , 'destroyIn Name Expression
    , 'introspect Self.Self


var Argument =
    , 'argumentExpression FullType Expression
    , 'argumentRecycle RawType [ Name ] Name


var Parameter =
    , 'parameterPattern FullType TA.Pattern
    , 'parameterRecycle RawType Name
    , 'parameterPlaceholder FullType Int



FunctionDef =
    {
    , lambdaRef as LambdaRef
    , context as Dict Name FullType
    , parameters [ Parameter ]
    , body as Expression
    , returnType as FullType
    }


RootDef =
    {
    , usr as USR
    , body as Expression
    , type as RawType
    }

LocalDef =
    {
    , body as Expression
    , pattern as Pattern
    , type as FullType
    }

