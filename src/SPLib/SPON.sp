union Outcome a =
    , Accepted [ FA.Statement ] a
    , Rejected (At Text)
    , Failed (At Text)


alias Reader a =
    fn [ FA.Statement ]: Outcome a


#
# Composition
#
onAcc as fn fn a: Reader b: fn Reader a: Reader b =
    fn chainedReaderB:
    fn readerA:
    fn statements:
    try readerA statements as
        , Accepted newStatements a: (chainedReaderB a) newStatements
        , Rejected reason: Rejected reason
        , Failed reason: Failed reason


return as fn a: Reader a =
    fn a:
    fn statements:
    Accepted statements a


getPos as fn [ FA.Statement ]: Pos =
    fn statements:
    try statements as
        , head :: tail: FA.statementPos head
        , []: Pos.End


reject as fn Text: Reader any =
    fn message:
    fn statements:
    Rejected << At (getPos statements) message


#
# Main
#
run as fn Reader a, Error.Module, [ FA.Statement ]: Res a =
    fn readerA, errorModule, statements:
    try readerA statements as
        , Accepted [] a: Ok a
        , Accepted (head :: tail) a: Error.res errorModule (FA.statementPos head) [ "unread statements" ]
        , Rejected (At pos r): Error.res errorModule pos [ r ]
        , Failed (At pos r): Error.res errorModule pos [ r ]


read as fn Reader a, Text, Text: Res a =
    fn reader, fsPath, content:
    errorModule as Error.Module =
        { content, fsPath }

    { errorModule, keepComments = False, stripLocations = False }
    >> Compiler/Parser.textToFormattableModule
    >> onOk (run reader errorModule __)


logHead as Reader None =
    fn statements:
    try statements as

        , head :: tail:
            log "LOG" head

            None

        , []:
            log "LOG" None

    Accepted statements None


#
# Terms
#

expr as fn Text, fn FA.Expr_: Maybe a: Reader a =
    fn aWhat, getA:
    try __ as

        , FA.Evaluation (FA.Expression _ p e) :: tail:
            try getA e as

                , Just a:
                    Accepted tail a

                , Nothing:
                    "Expecting " .. aWhat
                    >> At p __
                    >> Rejected

        , [ s ]:
            "Expecting " .. aWhat
            >> At (FA.statementPos s) __
            >> Rejected

        , _:
            Failed << At Pos.End "Expecting a statement"


text as Reader Text =
    try __ as
        , FA.LiteralText _ t: Just t
        , _: Nothing
    >> expr "a text literal" __


lowerName as Reader Text =
    try __ as
        , FA.Lowercase { attrPath = [], maybeModule = Nothing, maybeType = Nothing, name }: Just name
        , _: Nothing
    >> expr "a simple lowercase name" __


upperName as Reader Text =
    try __ as
        , FA.Uppercase { maybeModule = Nothing, name }: Just name
        , _: Nothing
    >> expr "a simple Uppercase name" __


constructor as Reader Text =
    try __ as
        , FA.Constructor { maybeModule = Nothing, name }: Just name
        , _: Nothing
    >> expr "a 'constructor name" __


#
# Higher rank
#

oneOf as fn [ Reader a ]: Reader a =
    fn readers:
    fn statements:
    try readers as

        , []:
            pos =
                try statements as
                    , head :: _: FA.statementPos head
                    , _: Pos.End

            Rejected << At pos "options exhausted"

        , headReader :: tail:
            try headReader statements as
                , Rejected _: (oneOf tail) statements
                , otherwise: otherwise


many as fn Reader a: Reader [ a ] =
    fn readerA:
    rec as fn [ a ]: Reader [ a ] =
        fn accum:
        fn statements:
        if statements == [] then
            [#
               Allowing emptiness at the right-hand side of `=` is ugly, but it's useful for commenting stuff out

               globalTypes =
                   # Meh
               globalValues = Blah

            #]
            Accepted [] (List.reverse accum)
        else
            try readerA statements as
                , Accepted tail a: (rec (a :: accum)) tail
                , Rejected e: Rejected e
                , Failed e: Failed e

    rec []


maybe as fn Reader a: Reader (Maybe a) =
    fn readerA:
    fn statements:
    try readerA statements as
        , Accepted tail a: Accepted tail (Just a)
        , Rejected _: Accepted statements Nothing
        , Failed r: Failed r


# HACK
expressionToStatements as fn FA.Expression: [ FA.Statement ] =
    fn e:
    try e as
        , FA.Expression _ _ (FA.Statements [ FA.Evaluation nested ]): expressionToStatements nested
        , FA.Expression _ _ (FA.Statements stats): stats
        , _: [ FA.Evaluation e ]


field as fn Text, Reader a: Reader a =
    fn fieldName, fieldReader:
    try __ as

        , FA.ValueDef
            {
            , body
            , nonFn
            , pattern =
                FA.Expression
                    _
                    pos
                    (FA.Lowercase
                         {
                         , attrPath = []
                         , maybeModule = Nothing
                         , maybeType = Nothing
                         , name
                         }
                    )
            }
        :: tail:
            if name == fieldName then
                try fieldReader (expressionToStatements body) as

                    , Accepted unreadStatements a:
                        try unreadStatements as
                            , []: Accepted tail a
                            , head :: _: Failed << At (FA.statementPos head) __ << "Could not make sense of all the statements in field `" .. fieldName .. "`."

                    , otherwise:
                        otherwise
            else
                Rejected << At pos __ << "expecting `" .. fieldName .. " =`"

        , head :: tail:
            Rejected << At (FA.statementPos head) "missing a simple assignment (ie `something = `)"

        , []:
            Rejected << At Pos.End "unexpected end of file"
