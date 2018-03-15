map <silent> <C-l> :call Comment()<CR>

function! Comment()
	if getline(".") =~ '^//'
		norm 0xx
	else
		norm 0i//
	endif
endfunction
