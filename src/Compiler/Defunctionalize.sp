Env =
    {
    , allDefsByUsr as Dict USR TA.ValueDef
    , currentConstraints as Dict LambdaSetId (Set LambdaRef)
    }




--------> At which point the free lambdaSetIds are populated with constraints from the caller?
                ----------> When specializing the call



specializeCall as fn Env, @State, LambdaRef, Context, [TA.Argument]: DA.Expression =
    fn env, @state, lambdaRef, context, args:

    try Hash.get 
    Get lambdaRef and its type
        populate all the lambdaSetIds with the constraints from context and args

    Rewrite the lambda with that specialization
    Store the lambda in a hash/dict by specialization



doCall as fn Env, TA.LambdaSetId, TA.Expression, [TA.Argument]: someReturn =
    fn env, setId, reference, args:

    if setId empty
        error with the algo

    else
        ```
        try reference as
            # the variants are determined by the lambdas in the set!
            # WE NEED TO ACTUALLY CREATE SPECIALIZED VERSIONS OF funA and funB, according to their context and args!
            # ---> Do I even have the lambdaSets in the context? (I need variables AND type!)
            #           ----------> They are in the reference type. Do I have the reference type? >_<
            'a a: $(specializeCall env funA a args)
            'b b: $(specializeCall env funB b args)
        ```









doExpression as fn Env, TA.Expression: someReturn =
    fn env, expression:
    try expression as
         TA.'literalNumber _ n:
            DA.'literalNumber _ n

         TA.'literalText _ text:
            DA.'literalNumber text

         TA.'constructor _ usr:
           DA.'constructor _ usr:

         TA.'variable _ ('refGlobal usr):
            if function then
                translate to the proper variant
            else
                DA.'globalVariable usr

         TA.'variable _ ('refLocal name):
            if function then
                translate to the proper variant
            else
                DA.'localVariable name

         TA.'fn _ LambdaSetId LambdaRef [ Parameter ] Expression FullType:
            extract to root
            replace with the correct variant in the lambdaSetId

         TA.'call _ lambdaSetId reference args:
            doCall env lambdaSetId reference args

         TA.'record _ maybeExt attrs:
            rec on maybeExt and attrs

         TA.'recordAccess _ name expression:
            rec on expression

         TA.'letIn letDef inExpr fullType:
            todo

         TA.'if _ { condition , false , true }: todo
         TA.'try _ { patternsAndExpressions , value , valueType }: todo
         TA.'destroyIn name expr: todo
         TA.'error _: todo
         TA.'introspect _: todo





doLocalDef as fn Env, TA.ValueDef: Result Text DA.LocalDef =
    fn env, taDef:
    todo "doLocalDef"


var Def =
    , 'native
    , 'error Text
    , 'done [ DA.RootDef ] [ DA.FunctionDef ]


doRootDef as fn Env, TA.ValueDef: Def =
    fn env, taDef:
    try taDef.body as
        'nothing: 'native
        'just body: doExpression env body
