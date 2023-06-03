[#

EmittableAst basically models a lower-level language, specifically one that does not have:


* Arbitrary let..ins: variables can only be declared inside blocks.
  (SP doesn't really allow arbitrary let..ins either...)

  This is because let..in can't be constructed in WGSL in any way.

  In JS they can be constructed with `(() => { let x; return f(x); })()`, but I assume it
  comes with a performance penalty, so probably good to get rid of these too?


* Pattern matching
  This is a very high-level feature, no language I'm considering as target supports it even remotely.


As of now, WGSL has also no support for higher-rank functions, functions declarations inside the body
of another function or pointer-to-functions; however we are not removing these features (yet?) from
EmittableAst because we are going to instead solve them by aggressive inlining.

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

