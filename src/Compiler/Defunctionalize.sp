Env =
    {
    , allDefsByUsr as Dict USR TA.ValueDef
    }


doExpression as fn Env, TA.Expression: blah =
    fn env, expression:
    try expression as
        todo: todo


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
