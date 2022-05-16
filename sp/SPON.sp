
union Outcome a =
    , Accepted [FA.Statement] a
    , Rejected (At Text)
    , Failed (At Text)


alias Reader a =
    [FA.Statement]: Outcome a


onAcc as (a: Reader b): Reader a: Reader b =
    chainedReaderB: readerA: statements:
    try readerA statements as
        Accepted newStatements a:
            chainedReaderB a newStatements

        Rejected reason:
            Rejected reason

        Failed reason:
            Failed reason


return as a: Reader a =
    a: statements:
    Accepted statements a


# HACK: I don't want to inject and Env in every function just to get the module name, at least for now
posEnd as Pos =
    Pos.End ""


unhackPosEnd as Text: Pos: Pos =
    moduleName: pos:
    try pos as
        Pos.End _: Pos.End moduleName
        _: pos


run as Reader a: Text: [FA.Statement]: Res a =
    readerA: sponName: statements:
    try readerA statements as
        Accepted [] a:
            Ok a

        Accepted (head :: tail) a:
            Error.res (FA.statementPos head) _: [ "unread statements" ]

        Rejected (At pos r):
            Error.res (unhackPosEnd sponName pos) _: [ r ]

        Failed (At pos r):
            Error.res (unhackPosEnd sponName pos) _: [ r ]


read as Reader a: Text: Text: Res a =
    reader: sponName: sponContent:
    sponContent
        >> Compiler/Lexer.lexer sponName
        >> onOk (Compiler/Parser.parse False sponName)
        >> onOk (run reader sponName)


logHead as Reader None =
    statements:
    try statements as
        head :: tail:
            log "LOG" head
            None
        []:
            log "LOG" None

    Accepted statements None


#
# Terms
#


text as Reader Text =
    statements:
    try statements as
        [ FA.Evaluation _ (FA.LiteralText pos t) ]:
            Accepted [] t

        [ s ]:
            Rejected << At (FA.statementPos s) "expecting a text literal"

        _:
            Failed << At posEnd "expecting a single statement"


upperName as Reader Text =
    statements:
    try statements as
        FA.Evaluation _ (FA.Constructor pos Nothing name) :: tail:
            Accepted tail name

        [ s ]:
            Rejected << At (FA.statementPos s) "expecting an Uppercase name"

        _:
            Failed << At posEnd "expecting a statement"


lowerOrUpperName as Reader Text =
    statements:
    try statements as
        (FA.Evaluation _ (FA.Variable pos Nothing name [])) :: tail:
            Accepted tail name

        (FA.Evaluation _ (FA.Constructor pos Nothing name)) :: tail:
            Accepted tail name

        [ s ]:
            Rejected << At (FA.statementPos s) "expecting an Uppercase or lowercase name"

        _:
            Failed << At posEnd "expecting a single statement"



#
# Higher level
#


oneOf as [Reader a]: Reader a =
    readers: statements:
    try readers as
        []:
            pos =
                try statements as
                    head :: _: FA.statementPos head
                    _: posEnd

            Rejected << At pos "options exhausted"

        headReader :: tail:
            try headReader statements as
                Rejected _:
                    oneOf tail statements

                otherwise:
                    otherwise


many as Reader a: Reader [a] =
    readerA:

    rec as [a]: Reader [a] =
        accum: statements:
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
                Accepted tail a:
                    rec (a :: accum) tail

                Rejected e:
                    Rejected e

                Failed e:
                    Failed e

    rec []


maybe as Reader a: Reader (Maybe a) =
    readerA: statements:
    try readerA statements as
        Accepted tail a:
            Accepted tail (Just a)

        Rejected _:
            Accepted statements Nothing

        Failed r:
            Failed r


field as Text: Reader a: Reader a =
    fieldName: fieldReader: statements:

    try statements as
        FA.Definition pos { body, pattern = FA.PatternAny _ False name Nothing, modifier, nonFn } :: tail:
            if name == fieldName then
                try fieldReader body as
                    Accepted unreadStatements a:
                        try unreadStatements as
                            []:
                                Accepted tail a

                            head :: _:
                                Failed << At (FA.statementPos head) << "Could not make sense of all the statements in field `" .. fieldName .. "`."

                    otherwise:
                        otherwise

            else
                Rejected << At pos << "expecting `" .. fieldName .. " =`"

        head :: tail:
            Rejected << At (FA.statementPos head) "missing a simple assignment (ie `something = `)"

        []:
            Rejected << At posEnd "unexpected end of file"
