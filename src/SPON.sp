
union Outcome a =
    , Accepted [FA.Statement] a
    , Rejected (At Text)
    , Failed (At Text)


alias Reader a =
    [FA.Statement]: Outcome a

#
# Composition
#
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


getPos as [FA.Statement]: Pos =
    statements:
    try statements as
        head :: tail: FA.statementPos head
        []: posEnd


reject as Text: Reader any =
    message: statements:
    Rejected << At (getPos statements) message


#
# Main
#

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
    reader: moduleName: sponContent:
    sponContent
        >> Compiler/Lexer.lexer moduleName
        >> onOk (Compiler/Parser.parse { moduleName, stripLocations = False })
        >> onOk (run reader moduleName)


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
        [ FA.Evaluation (FA.Expression _ (FA.LiteralText t)) ]:
            Accepted [] t

        [ s ]:
            Rejected << At (FA.statementPos s) "expecting a text literal"

        _:
            Failed << At posEnd "expecting a single statement"


word as Reader Token.Word =
    statements:
    try statements as
        FA.Evaluation (FA.Expression _ (FA.Variable { maybeType, word })) :: tail:
            Accepted tail word

        [ s ]:
            Rejected << At (FA.statementPos s) "expecting an Uppercase name"

        _:
            Failed << At posEnd "expecting a statement"


upperName as Reader Text =
    word >> onAcc w:

    try w as
        { modifier = Token.NameNoModifier , isUpper = True , maybeModule = Nothing , name , attrPath = [] }:
            return name
        _:
            reject "expecting an upper case name"


lowerOrUpperName as Reader Text =
    word >> onAcc w:

    try w as
        { modifier = Token.NameNoModifier , isUpper = _ , maybeModule = Nothing , name , attrPath = [] }:
            return name
        _:
            reject "expecting an upper or lower case name"



#
# Higher rank
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


# HACK
expressionToStatements as FA.Expression: [FA.Statement] =
    e:
    try e as
      FA.Expression _ (FA.Statements [ FA.Evaluation nested ]): expressionToStatements nested
      FA.Expression _ (FA.Statements stats): stats
      _: [FA.Evaluation e]


field as Text: Reader a: Reader a =
    fieldName: fieldReader: statements:

    try statements as
        FA.ValueDef
            {
            , body
            , nonFn
            , pattern =
                FA.Expression pos
                    (FA.Variable
                        {
                        , maybeType = Nothing
                        , word =
                            {
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
