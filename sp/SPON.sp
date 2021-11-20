
union Outcome a =
    , Accepted [FA.Statement] a
      # TODO add Pos
    , Rejected Text
      # TODO add Pos
    , Failed Text


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


run readerA statements =
    as Reader a: [FA.Statement]: Result Text a
    try readerA statements as
        Accepted [] a:
            Ok a

        Accepted rest a:
            # TODO add rest starting position
            Err "unread statements"

        Rejected r:
            Err r

        Failed r:
            Err r



#
# Terms
#


string statements =
    as Reader Text
    try statements as
        [ FA.Evaluation _ (FA.LiteralText pos text) ]:
            Accepted [] text

        [ _ ]:
            Rejected "expecting a text literal"

        _:
            Failed "expecting a single statement"


varName statements =
    as Reader Text
    try statements as
        (FA.Evaluation _ (FA.Variable pos args name)) :: tail:
            # TODO check args.isBinop?
            Accepted tail name

        [ _ ]:
            Rejected "expecting a list of variable or type names"

        _:
            Failed "expecting a single statement"



#
# Higher level
#


oneOf readers statements =
    as [Reader a]: Reader a
    try readers as
        []:
            Rejected "options exhausted"

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
                        if unreadStatements == []:
                            Accepted tail a

                        else
                            Failed << "Unread statements: " .. Debug.toHuman unreadStatements

                    otherwise:
                        otherwise

            else
                Rejected << "expecting `" .. fieldName .. " =`"

        _:
            Rejected "missing a simple assignment (ie `something = `)"
