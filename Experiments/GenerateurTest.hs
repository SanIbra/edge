import System.IO

name = "gentest"
extension=".ilpml"
keyword = ["let","and","if","in","then","else"]
constant=["True","False"]
ident="[a-zA-Z_][a-zA-Z0-9_]*"
int="[0-9]+"
paire= ("(",")")
commentLine="//"
commentBlock=("/*","*/")


emacskeyword key= "\\\\<\\\\("++(createRegex key)++")\\\\>"
createRegex [] ="\\"
createRegex (k:ks)=k++"\\\\|"++createRegex ks

emacsgene =do
   file<-openFile (name++".el") WriteMode
   hPutStrLn file $("(add-to-list 'auto-mode-alist '(\"\\\\"++extension++"\\\\'\" . "++name++"-mode)) " )--autoload
   hPutStrLn file ( "\n(defconst ilp-font-lock-keywords1(list")--description des keywords
   hPutStrLn file ( "'"++(emacskeyword keyword)++". font-lock-variable-name-face)")
   hPutStrLn file ( "'"++(emacskeyword constant)++". font-lock-constant-face)")
   hPutStrLn file ( "'"++ident++". font-lock-variable-name-face)")
   hPutStrLn file "))"
   hPutStrLn file ("(setq "++name++"-font-lock-keywords "++name++"-font-lock-keywords1)")
   hPutStrLn file ("\n(defun "++name++"-mode ()\n(interactive)\n(kill-all-local-variables)")--function qui defini le mode
   hPutStrLn file ("(set  (make-local-variable 'font-lock-defaults) '("++name++"-font-lock-keywords))")
   hPutStrLn file ("(setq major-mode '"++name++"-mode)")
   hPutStrLn file ("(setq mode-name \""++name++"\")")
   hPutStrLn file ("(run-hooks '"++name++"-mode-hook))")
   hClose file
   
vimgene = do
  file<-openFile (name++".vim") WriteMode
  hPutStrLn file $("syn keyword "++name++"Keyword "++(vimprint keyword))
  hPutStrLn file ("syn keyword  "++name++"Const   "++(vimprint constant))
  hPutStrLn file ("\nsyn   syn match "++name++"Ident display "++ident)
  hPutStrLn file ("syn region "++name++"CommentLine  start=\""++commentLine++"\" end=\"$\"")
  hPutStrLn file ("syn region "++name++"CommentBlock matchgroup="++name++"CommentBlock start=\"/"++(fst commentBlock)++"\"  end=\"\\"++(snd commentBlock)++"\" fold contains=ilpmlCommentBlockLevel")
  hPutStrLn file ("syn region "++name++"matchgroup="++name++"CommentBlock start=\"/"++(fst commentBlock)++"\"   end=\"\\"++(snd commentBlock)++"\" fold contains=ilpmlCommentBlockLevel contained")
 
  hPutStrLn file ("let b:current_syntax = \""++extension++"\"")
  hPutStrLn file ("syn match "++name++"Int display \""++int++"\"")
  hPutStrLn file ("hi def link "++name++"Keyword keyword")
  hPutStrLn file "hi def link   "++name++"CommentLine    Comment"
  hPutStrLn file "hi def link   "++name++"CommentBlock   Comment"
  hPutStrLn file "hi def link   "++name++"Const          Constant"
  hPutStrLn file "hi def link   "++name++"Ident          Identifier"
  hPutStrLn file "hi def link   "++name++"Int            Number"
  hPutStrLn file "hi def link   "++name++"CommentBlockLevel  Operator"
  hClose file
 
vimprint []=""
vimprint (x:xs)=x++" "++(vimprint xs)

extraction grammar 
