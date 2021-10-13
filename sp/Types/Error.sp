
alias Res a =
    Result Error a


union Error =
    , Simple Args
    , Nested [ Error ]


alias Env =
    {
#    , metaFile is MetaFile
#    , moduleByName is Dict Text { fsPath is Text, content is Text }
    }


alias Args =
    {
    , moduleName is Text
    , start is Number
    , end is Number
    , description is Env -> List ContentDiv
    }


union ContentDiv =
    , Text Text
    , CodeBlock Text
    , CodeBlockWithLineNumber Number [ Highlight ] [ Text ]


union Highlight =
    , HighlightWord { line is Number, colStart is Number, colEnd is Number }
    , HighlightBlock { lineStart is Number, lineEnd is Number }


res ea =
    is Args -> Res a

    ea >> Simple >> Err


text =
    is Text -> ContentDiv

    Text
