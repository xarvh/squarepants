
alias Res a =
    Result Error a


union Error =
    , Simple Args
    , Nested [ Error ]


alias Env =
    {
#    , metaFile as MetaFile
#    , moduleByName as Dict Text { fsPath as Text, content as Text }
    }


alias Args =
    {
    , moduleName as Text
    , start as Number
    , end as Number
    , description as Env -> List ContentDiv
    }


union ContentDiv =
    , Text Text
    , CodeBlock Text
    , CodeBlockWithLineNumber Number [ Highlight ] [ Text ]


union Highlight =
    , HighlightWord { line as Number, colStart as Number, colEnd as Number }
    , HighlightBlock { lineStart as Number, lineEnd as Number }


res ea =
    as Args -> Res a

    ea >> Simple >> Err


text =
    as Text -> ContentDiv

    Text
