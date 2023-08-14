
union Outcome a =
    , Accepted [FA.Statement] a
    , Rejected (Pos & Text)
    , Failed (Pos & Text)


alias Reader a =
    fn [FA.Statement]: Outcome a

#
# Composition
#
onAcc as fn (fn a: Reader b): fn Reader a: Reader b =
    fn chainedReaderB:
    fn readerA:
    fn statements:
    try readerA statements as
        , Accepted newStatements a:
            (chainedReaderB a) newStatements

        , Rejected reason:
            Rejected reason

        , Failed reason:
            Failed reason


return as fn a: Reader a =
    fn a: fn statements:
    Accepted statements a


getPos as fn [FA.Statement]: Pos =
    fn statements:
    try statements as
        , head :: tail: FA.statementPos head
        , []: Pos.End


reject as fn Text: Reader any =
    fn message:
    fn statements:
    Rejected << getPos statements & message


#
# Main
#
run as fn Reader a, Error.Module, [FA.Statement]: Res a =
    fn readerA, errorModule, statements:
    try readerA statements as
        , Accepted [] a:
            Ok a

        , Accepted (head :: tail) a:
            Error.res errorModule (FA.statementPos head) [ "unread statements" ]

        , Rejected (pos & r):
            Error.res errorModule pos [ r ]

        , Failed (pos & r):
            Error.res errorModule pos [ r ]


read as fn Reader a, Text, Text: Res a =
    fn reader, fsPath, content:

    errorModule as Error.Module =
        { content, fsPath }

    { errorModule, stripLocations = False }
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


text as Reader Text =
    fn statements:
    try statements as
        , [ FA.Evaluation (FA.Expression _ _ _ (FA.LiteralText t)) ]:
            Accepted [] t

        , [ s ]:
            Rejected << FA.statementPos s & "expecting a text literal"

        , _:
            Failed << Pos.End & "expecting a single statement"


word as Reader Token.Word =
    fn statements:
    try statements as
        , FA.Evaluation (FA.Expression _ _ _ (FA.Variable { maybeType, word = FA.Word _ _ w })) :: tail:
            Accepted tail w

        , [ s ]:
            Rejected << FA.statementPos s & "expecting an Uppercase name"

        , _:
            Failed << Pos.End & "expecting a statement"


upperName as Reader Text =
    word >> onAcc fn w:

    try w as
        , { modifier = Token.NameNoModifier , isUpper = True , maybeModule = Nothing , name , attrPath = [] }:
            return name
        , _:
            reject "expecting an upper case name"


lowerOrUpperName as Reader Text =
    word >> onAcc fn w:

    try w as
        , { modifier = Token.NameNoModifier , isUpper = _ , maybeModule = Nothing , name , attrPath = [] }:
            return name
        , _:
            reject "expecting an upper or lower case name"



#
# Higher rank
#


oneOf as fn [Reader a]: Reader a =
    fn readers:
    fn statements:
    try readers as
        , []:
            pos =
                try statements as
                    , head :: _: FA.statementPos head
                    , _: Pos.End

            Rejected << pos & "options exhausted"

        , headReader :: tail:
            try headReader statements as
                , Rejected _:
                    (oneOf tail) statements

                , otherwise:
                    otherwise


many as fn Reader a: Reader [a] =
    fn readerA:

    rec as fn [a]: Reader [a] =
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
                , Accepted tail a:
                    (rec (a :: accum)) tail

                , Rejected e:
                    Rejected e

                , Failed e:
                    Failed e

    rec []


maybe as fn Reader a: Reader (Maybe a) =
    fn readerA:
    fn statements:
    try readerA statements as
        , Accepted tail a:
            Accepted tail (Just a)

        , Rejected _:
            Accepted statements Nothing

        , Failed r:
            Failed r


# HACK
expressionToStatements as fn FA.Expression: [FA.Statement] =
    fn e:
    try e as
      , FA.Expression _ _ _ (FA.Statements [ FA.Evaluation nested ]): expressionToStatements nested
      , FA.Expression _ _ _ (FA.Statements stats): stats
      , _: [FA.Evaluation e]


field as fn Text, Reader a: Reader a =
    fn fieldName, fieldReader:
    fn statements:

    try statements as
        , FA.ValueDef
            {
            , body
            , nonFn
            , pattern =
                FA.Expression _ pos _
                    (FA.Variable
                        {
                        , maybeType = Nothing
                        , word =
                            FA.Word _ _ {
                              , modifier = Token.NameNoModifier
                              , isUpper = _
                              , maybeModule = Nothing
                              , name
                              , attrPath = []
                              }
                        }
                    )
            } :: tail:
                if name == fieldName then
                    try fieldReader (expressionToStatements body) as
                        , Accepted unreadStatements a:
                            try unreadStatements as
                                , []:
                                    Accepted tail a

                                , head :: _:
                                    Failed << (FA.statementPos head) & "Could not make sense of all the statements in field `" .. fieldName .. "`."

                        , otherwise:
                            otherwise

                else
                    Rejected << pos & "expecting `" .. fieldName .. " =`"

        , head :: tail:
            Rejected << FA.statementPos head & "missing a simple assignment (ie `something = `)"

        , []:
            Rejected << Pos.End & "unexpected end of file"

