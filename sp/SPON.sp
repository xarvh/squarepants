
union Outcome a =
    , Accepted [FA.Statement] a
    , Rejected (At Text)
    , Failed (At Text)


alias Reader a =
    [FA.Statement]: Outcome a


onAcc chainedReaderB readerA statements =
    as (a: Reader b): Reader a: Reader b
    try readerA statements as
        Accepted newStatements a:
            chainedReaderB a newStatements

        Rejected reason:
            Rejected reason

        Failed reason:
            Failed reason


return a statements =
    as a: Reader a
    Accepted statements a


# HACK: I don't want to inject and Env in every function just to get the module name, at least for now
posEnd =
    as Pos
    Pos.End ""


unhackPosEnd moduleName pos =
    as Text: Pos: Pos
    try pos as
        Pos.End _: Pos.End moduleName
        _: pos


run readerA sponName statements =
    as Reader a: Text: [FA.Statement]: Res a
    try readerA statements as
        Accepted [] a:
            Ok a

        Accepted (head :: tail) a:
            Error.res (FA.statementPos head) fn _: [ "unread statements" ]

        Rejected (At pos r):
            Error.res (unhackPosEnd sponName pos) fn _: [ r ]

        Failed (At pos r):
            Error.res (unhackPosEnd sponName pos) fn _: [ r ]


read reader sponName sponContent =
    as Reader a: Text: Text: Res a

    sponContent
        >> Compiler/Lexer.lexer sponName
        >> onOk (Compiler/Parser.parse False sponName)
        >> onOk (run reader sponName)


logHead statements =
    as Reader None

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


text statements =
    as Reader Text
    try statements as
        [ FA.Evaluation _ (FA.LiteralText pos t) ]:
            Accepted [] t

        [ s ]:
            Rejected << At (FA.statementPos s) "expecting a text literal"

        _:
            Failed << At posEnd "expecting a single statement"


upperName statements =
    as Reader Text
    try statements as
        FA.Evaluation _ (FA.Constructor pos Nothing name) :: tail:
            Accepted tail name

        [ s ]:
            Rejected << At (FA.statementPos s) "expecting an Uppercase name"

        _:
            Failed << At posEnd "expecting a statement"


lowerOrUpperName statements =
    as Reader Text
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


oneOf readers statements =
    as [Reader a]: Reader a
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


many readerA =
    as Reader a: Reader [a]

    rec accum statements =
        as [a]: Reader [a]
        if statements == []:
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


maybe readerA statements =
    as Reader a: Reader (Maybe a)
    try readerA statements as
        Accepted tail a:
            Accepted tail (Just a)

        Rejected _:
            Accepted statements Nothing

        Failed r:
            Failed r


field fieldName fieldReader statements =
    as Text: Reader a: Reader a

    try statements as
        FA.Definition pos { body, pattern = FA.PatternAny _ False name Nothing } :: tail:
            if name == fieldName:
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
