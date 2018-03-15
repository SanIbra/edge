
if exists("b:did_indent")
  finish
endif

let b:did_indent = 1

setlocal autoindent
setlocal indentexpr=MyIndent(v:lnum)
setlocal indentkeys+=0),=in,=else,=then

if exists("*GetIlpmlIndent")
  finish
endif

let s:INDENT_AFTER_KEYWORD= '\%(in\|then\|else\)\>\s*$'
let s:INDENT_AFTER_PAIRED_SYMBOL='^.*(\s*$'
let s:BLANK_LINE='^\s*$'

function! MyIndent(lnum)
	if has('syntax_items')
		let prevlinenum = s:get_last_normal_line(a:lnum - 1)
		if !prevlinenum
			return -1
		endif
		
		let line = s:get_line_trimmed(a:lnum) "line without comment and undesirable spaces

		let matchlinenum = s:get_matching_line(line, a:lnum)

		if matchlinenum
			return indent(matchlinenum)
		endif

		let prevline = s:get_line_trimmed(prevlinenum)
		let previndent = indent(prevlinenum)

		if prevline =~ s:INDENT_AFTER_KEYWORD || prevline =~ s:INDENT_AFTER_PAIRED_SYMBOL
			return previndent + &shiftwidth
		endif

		if s:get_line_trimmed(s:get_last_normal_line(prevlinenum - 1)) =~ s:INDENT_AFTER_KEYWORD
			return indent(prevlinenum - 1)
		endif
	endif
	return -1
endfunction

function! s:outdent(lnum)
	if s:get_line_trimmed(a:lnum - 1) =~ s:INDENT_AFTER_KEYWORD
		return s:outdent(a:lnum - 1)
	endif
	return indent(a:lnum)
endfunction

function! s:is_comment(lnum, col)
	return s:synthax_name(a:lnum, a:col) =~ "Comment"
endfunction

function! s:get_last_normal_line(lnum)
	let curlinenum = a:lnum

 	while curlinenum > 0
		if getline(curlinenum) !~ s:BLANK_LINE && !s:all_line_comment(curlinenum)
			return curlinenum
		endif

		let curlinenum -= 1
	endwhile

	return 0
endfunction

function! s:is_string(lnum, col)
	return s:synthax_name(a:lnum, a:col) =~ "String"
endfunction

function! s:search_pair(begin, end, lnum, score)
	if a:lnum == 0
		return 0
	endif
	if a:score == 0
		return a:lnum
	endif
	let line = s:get_line_trimmed(a:lnum)
	let regexbegin = a:begin.'\s*$'
	let regexend = a:end.'\s*$'
	if line =~ regexbegin
		 return s:search_pair(a:begin, a:end, s:get_last_normal_line(a:lnum - 1), a:score - 1)
	endif
	if line =~ regexend
		 return s:search_pair(a:begin, a:end, s:get_last_normal_line(a:lnum - 1), a:score + 1)
	endif
	return s:search_pair(a:begin, a:end, s:get_last_normal_line(a:lnum - 1),a:score)
endfunction

function! s:get_matching_line(line, lnum)
	let lastchar = a:line[strlen(a:line) - 1]

	if lastchar == ')'
		return s:search_pair('(', ')', s:get_last_normal_line(a:lnum - 1), 1)
	endif
	return 0
endfunction

function! s:all_line_comment(lnum)
	let line = getline(a:lnum)
	if line =~ '\s*//.*$' || line =~ '\s*/\*.*$' || line =~ '.*\*/\s*$'
		return 1
	endif
	if s:is_comment(a:lnum, 1) && s:is_comment(a:lnum, strlen(line) -1)
		return 1
	endif
	return 0
endfunction

function! s:synthax_name(lnum, col)
	return synIDattr(synID(a:lnum, a:col, 1), "name")
endfunction

function! s:get_line_trimmed(lnum)
	let line = getline(a:lnum)
	let line_len = strlen(line)
	if has('syntax_items')
		if  s:synthax_name(a:lnum, line_len) =~ 'ilpmlCommentLine' &&  s:synthax_name(1,line_len) !~ 'ilpmlCommentLine'
			let min = 1
			let max = strlen(line)
			while min < max
				let col = (min + max) / 2
				if  s:synthax_name(a:lnum, col) =~ 'ilpmlCommentLine'
					let max = col
				else
					let min = col + 1
				endif
			endwhile
			let line = strpart(line, 0, min - 1)
		endif
		return substitute(substitute(line, '\s*$', '', ''), '^\s*','','')
	else
		return substitute(substitute(line, '\s*//.*$', '', ''), '^\s*','','')
	endif
endfunction

function! TestIndent(curlinenum)
	let indent=indent(a:curlinenum - 1) + &shiftwidth
	return indent
endfunction
