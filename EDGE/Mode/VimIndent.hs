module Mode.VimIndent where
import Parseur.Regex
import Parseur.TypeStats

-- Special string only used during the generation of the indent file
regexindent::[(String,String)]->String
regexindent (x:[])=(fst x)++"[^"++(snd x)++"]*"
regexindent (x:xs)=(fst x)++"[^"++(snd x)++"]*\\|"++(regexindent xs) 

-- Same as vimprint in Vim.hs
listfermante::[String]->String
listfermante (x:[])=x
listfermante (x:xs)=x++","++(listfermante xs)
  
-- String of the beginning of the file (no information needed here)
debutIndent::String
debutIndent = "if exists(\"b:did_indent\")\n"
  ++ "  finish\n"
  ++ "endif\n"
  ++ "\n"
  ++ "let b:did_indent = 1\n"
  ++ "\n"
  ++ "setlocal autoindent\n"
  ++ "setlocal indentexpr=MyIndent(v:lnum)\n"
  ++ "\n"
  ++ "if exists(\"MyIndent\")\n"
  ++ "  finish\n"
  ++ "endif\n"
  ++ "\n"
  ++ "\" indent function\n"
  ++ "function! MyIndent(lnum)\n"
  ++ "\tif has('syntax_items')\n"
  ++ "\t\tlet prevlinenum = s:get_last_normal_line(a:lnum - 1)\n"
  ++ "\t\tif !prevlinenum\n"
  ++ "\t\t\treturn -1\n"
  ++ "\t\tendif\n"
  ++ "\t\t\n"
  ++ "\t\tlet line = s:get_line_trimmed(a:lnum)\n"
  ++ "\n"
  ++ "\t\tlet matchlinenum = s:get_matching_line(line, a:lnum)\n"
  ++ "\n"
  ++ "\t\tif matchlinenum != 0\n"
  ++ "\t\t\treturn indent(matchlinenum)\n"
  ++ "\t\tendif\n"
  ++ "\n"
  ++ "\t\tlet prevline = s:get_line_trimmed(prevlinenum)\n"
  ++ "\t\tlet previndent = indent(prevlinenum)\n"
  ++ "\n"
  ++ "\t\tif prevline =~ s:INDENT_AFTER_PAIRED_SYMBOL\n"
  ++ "\t\t\treturn previndent + &shiftwidth\n"
  ++ "\t\tendif\n"
  ++ "\tendif\n"
  ++ "\treturn -1\n"
  ++ "endfunction\n"
  ++ "\n"
  ++ "\" return true if the object line lnum and column col is a comment\n"
  ++ "function! s:is_comment(lnum, col)\n"
  ++ "\treturn s:syntax_name(a:lnum, a:col) =~ \"Comment\"\n"
  ++ "endfunction\n"
  ++ "\n"
  ++ "\" return the number of the last line that's not a blanck line or a comment only line"
  ++ "function! s:get_last_normal_line(lnum)\n"
  ++ "\tlet curlinenum = a:lnum\n"
  ++ "\n"
  ++ " \twhile curlinenum > 0\n"
  ++ "\t\tif getline(curlinenum) !~ s:BLANK_LINE && !s:all_line_comment(curlinenum)\n"
  ++ "\t\t\treturn curlinenum\n"
  ++ "\t\tendif\n"
  ++ "\n"
  ++ "\t\tlet curlinenum -= 1\n"
  ++ "\tendwhile\n"
  ++ "\n"
  ++ "\treturn 0\n"
  ++ "endfunction\n"
  ++ "\n"
  ++ "\" analysing function for the search_pair function\n"
  ++ "function! s:analyse_ligne(rbegin, rend, line, score)\n"
  ++ "\tif a:score == 0\n"
  ++ "\t\treturn 0\n"
  ++ "\tendif\n"
  ++ "\tif a:line =~ ('\\.*'.a:rbegin)\n"
  ++ "\t\tlet new_line = substitute(a:line,a:rbegin,\"\",\"\")\n"
  ++ "\t\treturn s:analyse_ligne(a:rbegin,a:rend, new_line, a:score - 1)\n"
  ++ "\tendif\n"
  ++ "\tif a:line =~ ('\\.*'.a:rend)\n"
  ++ "\t\tlet new_line = substitute(a:line,a:rend,\"\",\"\")\n"
  ++ "\t\treturn s:analyse_ligne(a:rbegin,a:rend, new_line, a:score + 1)\n"
  ++ "\tendif\n"
  ++ "\treturn a:score\n"
  ++ "endfunction\n\n"

-- main function (function called)
writeIndent::Stats->String
writeIndent stats =
  debutIndent
  ++ "setlocal indentkeys+="++(listfermante (map snd peers))++"\n"
  ++ "let s:INDENT_AFTER_PAIRED_SYMBOL='^.*\\("++(regexindent peers)++"\\)$'\n"
  ++ "let s:BLANK_LINE='^\\s*$'\n"
  ++ "\" backward searching function used in the get_matching_line function \n"
  ++ "function! s:search_pair(begin, end, lnum, score)\n"
  ++ "   if a:lnum == 0\n"
  ++ "     return 0\n"
  ++ "   endif\n"
  ++ "   let line = s:get_line_trimmed(a:lnum)\n"
  ++ "   let regexcontains = '^\\.*'.a:begin.'\\|'.a:end.'\\.*$'\n"
  ++ "   let regexbegin = a:begin.'[^()]*$'\n"
  ++ "   let regexend = a:end.'[^()]*$'\n"
  ++ "    if line =~ regexcontains\n"
  ++ "        let n_score = s:analyse_ligne(regexbegin,regexend,line,a:score)\n"
  ++ "        if n_score == 0\n"
  ++ "            return a:lnum\n"
  ++ "        endif\n"
  ++ "        return s:search_pair(a:begin, a:end, s:get_last_normal_line(a:lnum - 1),n_score)\n"
  ++ "    endif\n"
  ++ "    return s:search_pair(a:begin, a:end, s:get_last_normal_line(a:lnum - 1),a:score)\n"
  ++ "endfunction\n\n"
  ++ "\" if the last character of the line is a closing symbol, return the line of\n"
  ++ "\" the opening symbol paired with it, otherwise return 0\n"
  ++ "function! s:get_matching_line(line, lnum)\n"
  ++ "    let lastchar = a:line[strlen(a:line) - 1]\n"
  ++ "    let line_without_lc = substitute(a:line,\".$\",\"\",\"\")\n"
  ++ writecas peers
  ++ "    return 0\n"
  ++ "endfunction\n\n"
  ++ "\" return true if all the line is a comment\n"
  ++ "function! s:all_line_comment(lnum)\n"
  ++ "    let line = getline(a:lnum)\n"
  ++ "     if line =~ '\\s*"++ cl ++".*$' || line =~ '\\s*"++ cb1 ++").*$' || line =~ '.*"++ cb2 ++"\\s*$'\n"
  ++ "        return 1\n"
  ++ "    endif\n"
  ++ "    if s:is_comment(a:lnum, 1) && s:is_comment(a:lnum, strlen(line) -1)\n"
  ++ "        return 1\n"
  ++ "   endif\n"
  ++ "   return 0\n"
  ++ "endfunction\n\n"
  ++ "\" return the name of the syntax object at line lnum and column col\n"
  ++ "function! s:syntax_name(lnum, col)\n"
  ++ "    return synIDattr(synID(a:lnum, a:col, 1), \"name\")\n"
  ++ "endfunction\n\n"
  ++ "\" line without comment and undesirable spaces\n"
  ++ "function! s:get_line_trimmed(lnum)\n"
  ++ "   let line = getline(a:lnum)\n"
  ++ "   let line_len = strlen(line)\n"
  ++ "   if has('syntax_items')\n"
  ++ "      if  s:syntax_name(a:lnum, line_len) =~ '"++ name ++"CommentLine' &&  s:syntax_name(1,line_len) !~ '"++ name++"CommentLine'\n"
  ++ "         let min = 1\n"
  ++ "         let max = strlen(line)\n"
  ++ "         while min < max\n"
  ++ "            let col = (min + max) / 2\n"
  ++ "            if  s:syntax_name(a:lnum, col) =~ '"++ name ++"CommentLine'\n"
  ++ "                let max = col\n"
  ++ "            else\n"
  ++ "               let min = col + 1\n"
  ++ "            endif\n"
  ++ "            endwhile\n"
  ++ "           let line = strpart(line, 0, min - 1)\n"
  ++ "       endif\n"
  ++ "       return substitute(substitute(line, '\\s*$', '', ''), '^\\s*','','')\n"
  ++ "   else\n"
  ++ "       return substitute(substitute(line, '\\s*"++ cl ++".*$', '', ''), '^\\s*','','')\n"
  ++ "   endif\n"
  ++ "endfunction\n\n"
  where
    name = head (getName stats)
    peers = getPaire stats
    cb1 = toVim (fromStringtoReg (fst (head (getCommentBlock stats))))
    cb2 = toVim (fromStringtoReg (snd (head (getCommentBlock stats))))
    cl = toVim (fromStringtoReg (head (getCommentLine stats)))
   


-----------------------------------
-- return the string composed of all the cases of the peers in argument in the function s:get_matching_line
writecas:: [(String,String)]->String
writecas (x:[])= casident (fst x) (snd x)
writecas (x:xs) = 
   casident (fst x) (snd x)
   ++ writecas xs

-- return the case of the peer "ouvrante" "fermante" in the function s:get_matching_line
casident:: String ->String->String
casident ouvrante fermante = 
  "if lastchar == '"++fermante++"'\n"
  ++ "    let test = s:analyse_ligne(\""++ouvrante++"[^"++ouvrante++fermante++"]*$\",\""++fermante++"[^"++ouvrante++fermante++"]*$\",line_without_lc,1)\n"
  ++ "    if  test == 0\n"
  ++ "        return a:lnum\n"
  ++ "    endif\n"
  ++ "   return s:search_pair('"++ouvrante++"', '"++fermante++"', s:get_last_normal_line(a:lnum - 1), test)\n"
  ++ "endif\n"

-------------------------------------
