var Outcome a =
    , 'accepted [ FA.Statement ] a
    , 'rejected (At Text)
    , 'failed (At Text)


Reader a =
    fn [ FA.Statement ]: Outcome a


#
# Composition
#
onAcc as fn fn a: Reader b: fn Reader a: Reader b =
    fn chainedReaderB:
    fn readerA:
    fn statements:
    try readerA statements as
        'accepted newStatements a: (chainedReaderB a) newStatements
        'rejected reason: 'rejected reason
        'failed reason: 'failed reason


return as fn a: Reader a =
    fn a:
    fn statements:
    'accepted statements a


getPos as fn [ FA.Statement ]: Pos =
    fn statements:
    try statements as
        head :: tail: FA.statementPos head
        []: Pos.'end


reject as fn Text: Reader any =
    fn message:
    fn statements:
    'rejected << 'at (getPos statements) message


#
# Main
#
run as fn Reader a, Error.Module, [ FA.Statement ]: Res a =
    fn readerA, errorModule, statements:
    try readerA statements as
        'accepted [] a: 'ok a
        'accepted (head :: tail) a: Error.res errorModule (FA.statementPos head) [ "unread statements" ]
        'rejected ('at pos r): Error.res errorModule pos [ r ]
        'failed ('at pos r): Error.res errorModule pos [ r ]


read as fn Reader a, Text, Text: Res a =
    fn reader, fsPath, content:
    errorModule as Error.Module =
        { content, fsPath }

    { errorModule, keepComments = 'false, stripLocations = 'false }
    >> Compiler/Parser.textToFormattableModule
    >> onOk (run reader errorModule __)


logHead as Reader None =
    fn statements:
    try statements as

        head :: tail:
            log "LOG" head

            'none

        []:
            log "LOG" 'none

    'accepted statements 'none


#
# Terms
#

expr as fn Text, fn FA.Expr_: Maybe a: Reader a =
    fn aWhat, getA:
    try __ as

        FA.'evaluation (FA.'expression _ p e) :: tail:
            try getA e as

                'just a:
                    'accepted tail a

                'nothing:
                    "Expecting " .. aWhat
                    >> 'at p __
                    >> 'rejected

        [ s ]:
            "Expecting " .. aWhat
            >> 'at (FA.statementPos s) __
            >> 'rejected

        _:
            'failed << 'at Pos.'end "Expecting a statement"


text as Reader Text =
    try __ as
        FA.'literalText _ t: 'just t
        _: 'nothing
    >> expr "a text literal" __


lowerName as Reader Text =
    try __ as
        FA.'lowercase { attrPath = [], maybeModule = 'nothing, maybeType = 'nothing, name }: 'just name
        _: 'nothing
    >> expr "a simple lowercase name" __


upperName as Reader Text =
    try __ as
        FA.'uppercase { maybeModule = 'nothing, name }: 'just name
        _: 'nothing
    >> expr "a simple Uppercase name" __


constructor as Reader Text =
    try __ as
        FA.'constructor { maybeModule = 'nothing, name }: 'just name
        _: 'nothing
    >> expr "a 'constructor name" __


anyName as Reader Text =
    try __ as
        FA.'lowercase { attrPath = [], maybeModule = 'nothing, maybeType = 'nothing, name }: 'just name
        FA.'uppercase { maybeModule = 'nothing, name }: 'just name
        FA.'constructor { maybeModule = 'nothing, name }: 'just name
        _: 'nothing
    >> expr "a name" __



#
# Higher rank
#

oneOf as fn [ Reader a ]: Reader a =
    fn readers:
    fn statements:
    try readers as

        []:
            pos =
                try statements as
                    head :: _: FA.statementPos head
                    _: Pos.'end

            'rejected << 'at pos "options exhausted"

        headReader :: tail:
            try headReader statements as
                'rejected _: (oneOf tail) statements
                otherwise: otherwise


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
            'accepted [] (List.reverse accum)
        else
            try readerA statements as
                'accepted tail a: (rec (a :: accum)) tail
                'rejected e: 'rejected e
                'failed e: 'failed e

    rec []


maybe as fn Reader a: Reader (Maybe a) =
    fn readerA:
    fn statements:
    try readerA statements as
        'accepted tail a: 'accepted tail ('just a)
        'rejected _: 'accepted statements 'nothing
        'failed r: 'failed r


# HACK
expressionToStatements as fn FA.Expression: [ FA.Statement ] =
    fn e:
    try e as
        FA.'expression _ _ (FA.'statements [ FA.'evaluation nested ]): expressionToStatements nested
        FA.'expression _ _ (FA.'statements stats): stats
        _: [ FA.'evaluation e ]


field as fn Text, Reader a: Reader a =
    fn fieldName, fieldReader:
    try __ as

        FA.'valueDef
            {
            , body
            , nonFn
            , pattern =
                FA.'expression
                    _
                    pos
                    (FA.'lowercase
                         {
                         , attrPath = []
                         , maybeModule = 'nothing
                         , maybeType = 'nothing
                         , name
                         }
                    )
            }
        :: tail:
            if name == fieldName then
                try fieldReader (expressionToStatements body) as

                    'accepted unreadStatements a:
                        try unreadStatements as
                            []: 'accepted tail a
                            head :: _: 'failed << 'at (FA.statementPos head) __ << "Could not make sense of all the statements in field `" .. fieldName .. "`."

                    otherwise:
                        otherwise
            else
                'rejected << 'at pos __ << "expecting `" .. fieldName .. " =`"

        head :: tail:
            'rejected << 'at (FA.statementPos head) "missing a simple assignment (ie `something = `)"

        []:
            'rejected << 'at Pos.'end "unexpected end of file"
