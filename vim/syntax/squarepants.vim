
" Keywords
syn keyword spKeywords fn try as var with else if then :

" Operators
syn keyword spOperator and or
syn match spOperator "\([-!%&\*\+./<=>\?@\\^:]\|\<_\>\)"

syn match spType "\<[A-Z][0-9A-Za-z_]*"
syn match spConstructor "['][0-9A-Za-z_]*"

" Delimiters
syn match spDelimiter  "[,]"
syn match spBraces  "[()[\]{}]"

" Comments
syn keyword spTodo TODO HACK XXX contained
syn match spLineComment "#.*" contains=spTodo,@spell
syn region spBlockComment matchgroup=spComment start="\[#|\=" end="#\]" contains=spTodo,spComment,@spell fold

" Strings
syn match spStringEscape "\\u[0-9a-fA-F]\{4}" contained
syn match spStringEscape "\\[nrfvbt\\\"]" contained
syn region spSingleQuote start="\"" skip="\\\"" end="\"" contains=spStringEscape,@spell
syn region spTripleQuote start="\"\"\"" skip="\\\"" end="\"\"\"" contains=spStringEscape,@spell

" Numbers
syn match spInt "-\?\<\d\+\>\|0[xX][0-9a-fA-F]\+\>"
syn match spFloat "\(\<\d\+\.\d\+\>\)"

" Folding
"syn region spTopLevelTypedef start="type" end="\n\(\n\n\)\@=" contains=ALL fold
"syn region spTopLevelFunction start="^[a-zA-Z].\+\n[a-zA-Z].\+=" end="^\(\n\+\)\@=" contains=ALL fold
"syn region spCaseBlock matchgroup=spCaseBlockDefinition start="^\z\(\s\+\)\<try\>" end="^\z1\@!\W\@=" end="\(\n\n\z1\@!\)\@=" end="\n\z1\@!\(\n\n\)\@=" contains=ALL fold
"syn region spCaseItemBlock start="^\z\(\s\+\).\+:$" end="^\z1\@!\W\@=" end="\(\n\n\z1\@!\)\@=" end="\(\n\z1\S\)\@=" contains=ALL fold

hi def link spKeywords      PreProc

hi def link spOperator      Statement
hi def link spType          Type
hi def link spConstructor   Identifier
hi def link spDelimiter     Delimiter
hi def link spBraces        Delimiter

hi def link spTodo          Todo
hi def link spLineComment   Comment
hi def link spBlockComment  Comment

hi def link spStringEscape  Special
hi def link spSingleQuote   String
hi def link spTripleQuote   String

hi def link spInt           Number
hi def link spFloat         Number

syn sync minlines=500
