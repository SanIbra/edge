module VimIdent where
import System.IO
import ParseEdge

motReserver = "\\*.+^?()|[]{}$"
regexComment []=[]
regexComment (c:r)=if (any (== c) motReserver) then '\\':c:(regexComment r) else c:(regexComment r)

regexindent (x:[])=(fst x)++"[^"++(snd x)++"]*"
regexindent (x:xs)=(fst x)++"[^"++(snd x)++"]*\\|"++(regexindent xs) 

listfermante (x:[])=x
listfermante (x:xs)=x++","++(listfermante xs)
  
writeIndent file fichier=do
   debutIdent<-(openFile "debutVimIdent" ReadMode)
   contents<-hGetContents debutIdent
   hPutStrLn file contents
   nom<-(genName fichier)
   peers<-(genPaire fichier)
   cb<-(genCommentBlock fichier)
   cl<-(genCommentLine fichier)

   hPutStrLn file ("setlocal indentkeys+="++(listfermante (map snd peers)))
   hPutStrLn file ("let s:INDENT_AFTER_PAIRED_SYMBOL='^.*\\("++(regexindent peers)++"\\)$'")
   hPutStrLn file "let s:BLANK_LINE='^\\s*$'"

   hPutStrLn file "function! s:search_pair(begin, end, lnum, score)"
   hPutStrLn file "   if a:lnum == 0"
   hPutStrLn file "     return 0"
   hPutStrLn file "   endif"
   hPutStrLn file "   let line = s:get_line_trimmed(a:lnum)"
   hPutStrLn file "   let regexcontains = '^\\.*'.a:begin.'\\|'.a:end.'\\.*$'"


   hPutStrLn file "   let regexbegin = a:begin.'[^()]*$'"
   hPutStrLn file "   let regexend = a:end.'[^()]*$'"


   hPutStrLn file "    if line =~ regexcontains"
   hPutStrLn file "        let n_score = s:analyse_ligne(regexbegin,regexend,line,a:score)"
   hPutStrLn file "        if n_score == 0"
   hPutStrLn file "            return a:lnum"
   hPutStrLn file "        endif"
   hPutStrLn file "        return s:search_pair(a:begin, a:end, s:get_last_normal_line(a:lnum - 1),n_score)"
   hPutStrLn file "    endif"
   hPutStrLn file "    return s:search_pair(a:begin, a:end, s:get_last_normal_line(a:lnum - 1),a:score)"
   hPutStrLn file "endfunction"

   hPutStrLn file "function! s:get_matching_line(line, lnum)"
   hPutStrLn file "    let lastchar = a:line[strlen(a:line) - 1]"
   hPutStrLn file "    let line_without_lc = substitute(a:line,\".$\",\"\",\"\")"

   writecas file peers


   hPutStrLn file "    return 0"
   hPutStrLn file "endfunction"

   hPutStrLn file "function! s:all_line_comment(lnum)"
   hPutStrLn file "    let line = getline(a:lnum)"


   hPutStrLn file ("     if line =~ '\\s*"++(regexComment (head cl))++".*$' || line =~ '\\s*"++(regexComment (fst (head cb)))++").*$' || line =~ '.*"++(regexComment (snd(head cb)))++"\\s*$'")


   hPutStrLn file "        return 1"
   hPutStrLn file "    endif"
   hPutStrLn file "    if s:is_comment(a:lnum, 1) && s:is_comment(a:lnum, strlen(line) -1)"
   hPutStrLn file "        return 1"
   hPutStrLn file "   endif"
   hPutStrLn file "   return 0"
   hPutStrLn file "endfunction"

   hPutStrLn file "function! s:synthax_name(lnum, col)"
   hPutStrLn file "    return synIDattr(synID(a:lnum, a:col, 1), \"name\")"
   hPutStrLn file "endfunction"

   hPutStrLn file "function! s:get_line_trimmed(lnum)"
   hPutStrLn file "   let line = getline(a:lnum)"
   hPutStrLn file "   let line_len = strlen(line)"
   hPutStrLn file "   if has('syntax_items')"
   hPutStrLn file ("      if  s:synthax_name(a:lnum, line_len) =~ '"++(head nom)++"CommentLine' &&  s:synthax_name(1,line_len) !~ '"++(head nom)++"CommentLine'")
   hPutStrLn file "         let min = 1"
   hPutStrLn file "         let max = strlen(line)"
   hPutStrLn file "         while min < max"
   hPutStrLn file "            let col = (min + max) / 2"
   hPutStrLn file ("            if  s:synthax_name(a:lnum, col) =~ '"++(head nom)++"CommentLine'")
   hPutStrLn file "                let max = col"
   hPutStrLn file "            else"
   hPutStrLn file "               let min = col + 1"
   hPutStrLn file "            endif"
   hPutStrLn file "            endwhile"
   hPutStrLn file "           let line = strpart(line, 0, min - 1)"
   hPutStrLn file "       endif"
   hPutStrLn file "       return substitute(substitute(line, '\\s*$', '', ''), '^\\s*','','')"
   hPutStrLn file "   else"


   hPutStrLn file ("       return substitute(substitute(line, '\\s*"++(regexComment (head cl))++".*$', '', ''), '^\\s*','','')")


   hPutStrLn file "   endif"
   hPutStrLn file "endfunction"
   
   hClose debutIdent
   


-----------------------------------
writecas file (x:[])=do casident file (fst x) (snd x)
writecas file (x:xs) =do 
   casident file (fst x) (snd x)
   writecas file xs

casident file ouvrante fermante= do
   hPutStrLn file ("if lastchar == '"++fermante++"'")
   hPutStrLn file ("    let test = s:analyse_ligne(\""++ouvrante++"[^"++ouvrante++fermante++"]*$\",\""++fermante++"[^"++(ouvrante++fermante)++"]*$\",line_without_lc,1)")
   hPutStrLn file "    if  test == 0"
   hPutStrLn file "        return a:lnum"
   hPutStrLn file "    endif"
   hPutStrLn file ("   return s:search_pair('"++ouvrante++"', '"++fermante++"', s:get_last_normal_line(a:lnum - 1), test)")
   hPutStrLn file "endif"

-------------------------------------
test =do
  file<-(openFile "test" WriteMode)
  b<-(genPaire "edge.edge")
  writeIndent file "edge.edge"
  hClose file

test2 =do
  file<-(openFile "debutVimIdent" ReadMode)
  contents<-hGetContents file
  putStrLn contents