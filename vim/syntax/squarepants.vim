"
" TODO Hastely copied from elm-vim and tweaked, needs a serious rewrite
"

" Keywords
syn keyword spConditional else if then
syn keyword spAlias alias
syn keyword spTypedef fn try as is union with

" Operators
syn keyword spOperator and or
syn match spOperator contained "\([-!#$%`&\*\+./<=>\?@\\^|~:]\|\<_\>\)"

" Types
syn match spType "\<[A-Z][0-9A-Za-z_'-]*"
syn keyword spNumberType number

" Delimiters
syn match spDelimiter  "[,]"
syn match spBraces  "[()[\]{}]"

" Functions
syn match spTupleFunction "\((,\+)\)"

" Comments
syn keyword spTodo TODO HACK XXX contained
syn match spLineComment "#.*" contains=spTodo,@spell
syn region spComment matchgroup=spComment start="\[#|\=" end="#\]" contains=spTodo,spComment,@spell fold

" Strings
syn match spStringEscape "\\u[0-9a-fA-F]\{4}" contained
syn match spStringEscape "\\[nrfvbt\\\"]" contained
syn region spString start="\"" skip="\\\"" end="\"" contains=spStringEscape,@spell
syn region spTripleString start="\"\"\"" skip="\\\"" end="\"\"\"" contains=spStringEscape,@spell
syn match spChar "'[^'\\]'\|'\\.'\|'\\u[0-9a-fA-F]\{4}'"

" Numbers
syn match spInt "-\?\<\d\+\>\|0[xX][0-9a-fA-F]\+\>"
syn match spFloat "\(\<\d\+\.\d\+\>\)"

" Identifiers
syn match spTopLevelDecl "^\s*[a-zA-Z][a-zA-z0-9_]*\('\)*\s\+:\(\r\n\|\r\|\n\|\s\)\+" contains=spOperator
syn match spFuncName /^\l\w*/

" Folding
syn region spTopLevelTypedef start="type" end="\n\(\n\n\)\@=" contains=ALL fold
syn region spTopLevelFunction start="^[a-zA-Z].\+\n[a-zA-Z].\+=" end="^\(\n\+\)\@=" contains=ALL fold
"syn region spCaseBlock matchgroup=spCaseBlockDefinition start="^\z\(\s\+\)\<try\>" end="^\z1\@!\W\@=" end="\(\n\n\z1\@!\)\@=" end="\n\z1\@!\(\n\n\)\@=" contains=ALL fold
syn region spCaseItemBlock start="^\z\(\s\+\).\+:$" end="^\z1\@!\W\@=" end="\(\n\n\z1\@!\)\@=" end="\(\n\z1\S\)\@=" contains=ALL fold

hi def link spFunction Special
hi def link spFuncName Function
hi def link spCaseBlockDefinition Conditional
hi def link spCaseBlockItemDefinition Conditional
hi def link spLetBlockDefinition TypeDef
hi def link spTopLevelDecl Function
hi def link spTupleFunction Normal
hi def link spTodo Todo
hi def link spComment Comment
hi def link spLineComment Comment
hi def link spString String
hi def link spTripleString String
hi def link spChar String
hi def link spStringEscape Special
hi def link spInt Number
hi def link spFloat Float
hi def link spDelimiter Delimiter
hi def link spBraces Delimiter
hi def link spTypedef TypeDef
hi def link spImport Include
hi def link spConditional Conditional
hi def link spAlias Delimiter
hi def link spOperator Operator
hi def link spType Identifier
hi def link spNumberType Identifier

syn sync minlines=500
