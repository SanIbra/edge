





syn keyword	ilpmlKeyword		let and if in then else	
syn keyword	ilpmlConst		true false


syn match ilpmlOperator display "+\|-\|*\|/\|%\|!\|<\|<=\|>\|>=\|==\|!=\||\|^"


syn match ilpmlEscape display contained /\\./
syn region ilpmlString start=+"+ end=+"+ contains=ilpmlEscape

syn match ilpmlInt display "[0-9]+"
syn match ilpmlFloat display "[0-9]* '.' [0-9]*"
syn match ilpmlIdent display "[a-zA-Z][a-zA-Z0-9]*"

syn region ilpmlCommentLine	start="//"	end="$"
syn region ilpmlCommentBlock matchgroup=ilpmlCommentBlock start="/\*"	end="\*/" fold contains=ilpmlCommentBlockLevel
syn region ilpmlCommentBlockLevel matchgroup=ilpmlCommentBlock start="/\*" end="\*/" contains=ilpmlCommentBlockLevel contained

let b:current_syntax = "ilpml"
hi def link	ilpmlKeyword		Keyword
hi def link	ilpmlCommentLine	Comment
hi def link	ilpmlCommentBlock	Comment
hi def link	ilpmlConst		Constant
hi def link 	ilpmlIdent		Identifier
hi def link	ilpmlInt		Number
hi def link	ilpmlFloat		Number
hi def link	ilpmlOperator		Operator
hi def link	ilpmlString		String
hi def link	ilpmlCommentBlockLevel	Operator

