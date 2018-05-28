module Vim where
import VimIdent
import System.IO
import System.Directory
import ParseEdge


     

vimgene fichier= do
  nom<-(genName fichier)
  createDirectory "vim"
  createDirectory ("vim/"++(head nom)++".vim")
  createDirectory ("vim/"++(head nom)++".vim/colors")
  createDirectory ("vim/"++(head nom)++".vim/compiler")
  createDirectory ("vim/"++(head nom)++".vim/doc")
  createDirectory ("vim/"++(head nom)++".vim/ftdetect")
  createDirectory ("vim/"++(head nom)++".vim/ftplugin")
  createDirectory ("vim/"++(head nom)++".vim/indent")
  createDirectory ("vim/"++(head nom)++".vim/keymap")
  createDirectory ("vim/"++(head nom)++".vim/plugin")
  createDirectory ("vim/"++(head nom)++".vim/syntax")
  file <-openFile("vim/"++(head nom)++".vim/ftdetect/"++(head nom)++".vim") WriteMode
  ext<-(genExtension fichier)
  hPutStrLn file ("au BufRead,BufNewFile *"++(head ext)++" set filetype="++(head nom))
  hClose file
  syntaxFile<-openFile ("vim/"++(head nom)++".vim/syntax/"++(head nom)++".vim") WriteMode
  syntax syntaxFile fichier 
  syntaxfin syntaxFile fichier
  indent<-(openFile ("vim/"++(head nom)++".vim/indent/"++(head nom)++".vim") WriteMode)
  writeIndent indent fichier
  hClose indent
  hClose syntaxFile

  
syntax syntaxFile fichier=do
  kw<-(genKeyword fichier)
  cts<-(genConstant fichier)
  op<-(genOperateur fichier)
  id<-(genIdent fichier)
  num<-(genInt fichier)
  cl<-(genCommentLine fichier)
  cb<-(genCommentBlock fichier)
  if ((head kw)=="") then hPutStr syntaxFile "" else vimKeywords syntaxFile fichier
  if ((head cts)=="") then hPutStr syntaxFile "" else vimConstants syntaxFile fichier
  if ((head op)=="") then hPutStr syntaxFile "" else vimOperators syntaxFile fichier
  if ((head id)=="") then hPutStr syntaxFile "" else vimIdents syntaxFile fichier
  if ((head num)=="") then hPutStr syntaxFile "" else vimNumbers syntaxFile fichier
  if ((head cl)=="") then hPutStr syntaxFile "" else vimCommentLines syntaxFile fichier
  if ((fst(head cb))=="") then hPutStr syntaxFile "" else vimCommentBlocks syntaxFile fichier
  vimString syntaxFile fichier

syntaxfin syntaxFile fichier=do
  nom<-(genName fichier)
  kw<-(genKeyword fichier)
  cts<-(genConstant fichier)
  op<-(genOperateur fichier)
  id<-(genIdent fichier)
  num<-(genInt fichier)
  cl<-(genCommentLine fichier)
  cb<-(genCommentBlock fichier)
  hPutStrLn syntaxFile ("let b:current_syntax = \""++(head nom)++"\"")
  if ((head kw)=="") then hPutStr syntaxFile "" else hPutStrLn syntaxFile ("hi def link "++(head nom)++"Keyword  Keyword")
  if ((head cl)=="") then hPutStr syntaxFile "" else hPutStrLn syntaxFile ("hi def link "++(head nom)++"CommentLine  Comment")
  if ((fst(head cb))=="") then hPutStr syntaxFile "" else hPutStrLn syntaxFile ("hi def link "++(head nom)++"CommentBlock  Comment")
  if ((head cts)=="") then hPutStr syntaxFile "" else hPutStrLn syntaxFile ("hi def link "++(head nom)++"Const  Constant")
  if ((head id)=="") then hPutStr syntaxFile "" else hPutStrLn syntaxFile ("hi def link "++(head nom)++"Ident  Identifier")
  if ((head num)=="") then hPutStr syntaxFile "" else hPutStrLn syntaxFile ("hi def link "++(head nom)++"Int   Number")
  if ((head op)=="") then hPutStr syntaxFile "" else hPutStrLn syntaxFile ("hi def link "++(head nom)++"Operator  Operator")
  hPutStrLn syntaxFile ("hi def link "++(head nom)++"String  String")
  if ((fst(head cb))=="") then hPutStr syntaxFile "" else hPutStrLn syntaxFile ("hi def link "++(head nom)++"CommentBlockLevel  Operator")

  

vimKeywords syntaxFile fichier=do 
  nom<-(genName fichier)
  kword<-(genKeyword fichier)
  hPutStrLn syntaxFile  ("syn keyword "++(head nom)++"Keyword "++(vimprint kword  " "))

vimConstants syntaxFile fichier=do 
  nom<-(genName fichier)
  cts<-(genConstant fichier)
  hPutStrLn syntaxFile ("syn keyword  "++(head nom)++"Const   "++(vimprint cts " "))

vimOperators syntaxFile fichier=do 
  nom<-(genName fichier)
  op<-(genOperateur fichier)
  hPutStrLn syntaxFile ("syn match "++(head nom)++"Operator display \""++(vimprint op "|")++"\"")

vimIdents  syntaxFile fichier=do 
  nom<-(genName fichier)
  id<-(genIdent fichier)
  hPutStrLn syntaxFile ("syn match "++(head nom)++"Ident display \""++(head id)++"\"")

vimNumbers  syntaxFile fichier=do 
  nom<-(genName fichier)
  int<-(genInt fichier)
  hPutStrLn syntaxFile ("syn match "++(head nom)++"Int display \""++(head int)++"\"")

vimCommentLines  syntaxFile fichier=do 
  nom<-(genName fichier)
  cl<-(genCommentLine fichier)
  hPutStrLn syntaxFile ("syn region "++(head nom)++"CommentLine start=\""++(head cl)++"\" end=\"$\"")

vimCommentBlocks  syntaxFile fichier=do 
  nom<-(genName fichier)
  cb<-(genCommentBlock fichier)
  hPutStrLn syntaxFile ("syn region "++(head nom)++"CommentBlock matchgroup="++(head nom)++"CommentBlock start=\""++(regexComment (fst (head cb)))++"\" end=\""++(regexComment(snd (head cb)))++"\" fold contains="++(head nom)++"CommentBlockLevel\n")
  hPutStrLn syntaxFile ("syn region "++(head nom)++"CommentBlockLevel matchgroup="++(head nom)++"CommentBlock start=\""++(regexComment (fst (head cb)))++"\" end=\""++(regexComment(snd (head cb)))++"\" contains="++(head nom)++"CommentBlockLevel contained")
  
vimString syntaxFile fichier=do
  nom<-(genName fichier)
  hPutStrLn syntaxFile ("syn match "++(head nom)++"Escape display contained /\\./")
  hPutStrLn syntaxFile ("syn region "++(head nom)++"String start=+\"+ end=+\"+ contains="++(head nom)++"Escape")

vimprint (x:[]) sep=x
vimprint (x:xs) sep=x++sep++(vimprint xs sep)