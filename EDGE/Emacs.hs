module Emacs where
import System.IO
import System.Directory
import ParseEdge
emacskeyword key= "\\\\<\\\\("++(createRegex key)++")\\\\>"
createRegex (k:[]) =k++"\\\\"
createRegex (k:ks)=k++"\\\\|"++createRegex ks

emacsMode fichier =do
   nom<-(genName fichier)
   ext<-(genExtension fichier)
   file<-openFile ((head nom)++".el") WriteMode
   headFile file fichier
   coloration file fichier
   commentSyntax file fichier
   identation file fichier
   keymapping file fichier
   fin file fichier
   hClose file
    
headFile file fichier= do
    nom<-(genName fichier)
    ext<-(genExtension fichier)
    hPutStrLn file ";;Commande a ajouter dans le .emacs pour que le mode se charge automatiquement à l'ouverture de emacs"
    hPutStrLn file (";;(autoload '"++(head nom)++"-mode \"chemin du mode.el\" \""++(head nom)++" Mode editing\" t)")
    hPutStrLn file (";;(add-to-list 'auto-mode-alist '(\"\\\\"++(head ext)++"\\\\'\" . "++(head nom)++"-mode)) \n" )

coloration file fichier =do
    nom<-(genName fichier)
    kw<-(genKeyword fichier)
    cts<-(genConstant fichier)
    id<-(genIdent fichier)

    hPutStrLn file ( "\n(defconst "++(head nom)++"-font-lock-keywords1(list")--description des keywords
    if ((head kw)=="") then hPutStr file "" else   keywords file fichier
    if ((head cts)=="") then hPutStr file "" else constants file fichier
    if ((head id)=="") then hPutStr file "" else idents file fichier 
    hPutStrLn file "))"
    hPutStrLn file ("(setq "++(head nom)++"-font-lock-keywords "++(head nom)++"-font-lock-keywords1)")

keywords file fichier= do 
    kw<-(genKeyword fichier)
    hPutStrLn file ( "  '(\""++(emacskeyword kw)++"\". font-lock-keyword-name-face)")

constants file fichier= do
    cts<-(genConstant fichier)
    hPutStrLn file ( "  '(\""++(emacskeyword cts)++"\". font-lock-constant-face)")

idents file fichier =do
    id<-(genIdent fichier)
    hPutStrLn file ( "  '(\""++(head id)++"\". font-lock-variable-name-face)")

keymapping file fichier=do
    nom<-(genName fichier)
    hPutStrLn file (";Permet de faire le keyBinding ")
    hPutStrLn file ("(defvar "++(head nom)++"-mode-map")
    hPutStrLn file ("  (let (("++(head nom)++"-mode-map (make-keymap))))")
    hPutStrLn file (";    (define-key "++(head nom)++"-mode-map \"\\C-j\" 'newline-and-indent) la commande CTRL+J va appliquer la fonction newligne-and-indent)")
    hPutStrLn file ((head nom)++"-mode-map)")

--Ecriture des commmentaires 
traiter [] el = False
traiter (x:xs) el =  if el==x 
                        then True else traiter xs el

--forme un string des n premier element de s
couperString s 0=[]
couperString [] n=[]
couperString (x:xs) n=x:(couperString xs (n-1))


comL _ [] _=[]
comL c tt 0=if c==(tt!!0) then '1':(comL c tt 1) else (comL c tt 1)
comL c tt 1=if c==(tt!!1) then '2':(comL c tt 2) else (comL c tt 2)
comL c tt 2=if ( (c==(tt!!2)) && not (c==(tt!!0)) ) then '1':(comL c tt 3) else (comL c tt 3)
comL c tt 3=if ( (c==(tt!!3))  && not (c==(tt!!1)) ) then '2':(comL c tt 4) else (comL c tt 4)
comL c tt 4=if c==(tt!!4) then '3':(comL c tt 5) else (comL c tt 5)
comL c tt 5=if c==(tt!!5) then '4':[] else []

commentSyntax file fichier=do
    cb<-(genCommentBlock fichier)
    cl<-(genCommentLine fichier)
    nom<-(genName fichier)
    hPutStrLn file ("(defvar "++(head nom)++"-mode-syntax-table")
    hPutStrLn file ("  (let (("++(head nom)++"-mode-syntax-table (make-syntax-table)))")
    if ((head cl)=="")  then hPutStr file "" else commentLines file fichier
    if ((fst(head cb))=="") then hPutStr file "" else commentBlocks file fichier
    hPutStrLn file ("  "++(head nom)++"-mode-syntax-table)")
    hPutStrLn file ("\"Syntax table for "++(head nom)++"-mode\")")
    
comment c cl cb1 cb2 n tr=if (traiter (couperString (cl++cb1++cb2) tr) c) then "" else (comL c (cl++cb1++cb2) 0)


commentLines file fichier=do
    cl<-(genCommentLine fichier)
    cb<-(genCommentBlock fichier)
    nom<-(genName fichier)
    if ( (comment ((head cl)!!0) (head cl) (fst (head cb)) (snd (head cb)) 0 1) =="")
        then hPutStr file ""
        else hPutStrLn file ("    (modify-syntax-entry ?"++(((head cl)!!0):[])++" \". "++(comment ((head cl)!!0) (head cl) (fst (head cb)) (snd (head cb)) 0 1)++  " \" "++(head nom)++"-mode-syntax-table)")
    hPutStrLn file ("    (modify-syntax-entry ?"++(((head cl)!!1):[])++" \". "++(comment ((head cl)!!1) (head cl) (fst (head cb)) (snd (head cb)) 1 0)++  "b\" "++(head nom)++"-mode-syntax-table)")
    hPutStrLn file ("    (modify-syntax-entry ?\\n \"> b\" "++(head nom)++"-mode-syntax-table)")
        
writeCommentBlock file cl cb1 cb2 nom=do
    if ( (comment (cb1!!0) cl cb1 cb2 2 2) =="")
        then hPutStr file ""
        else hPutStrLn file ("    (modify-syntax-entry ?"++((cb1!!0):[])++" \". "++(comment (cb1!!0) cl cb1 cb2 2 2) ++  "\" "++nom++"-mode-syntax-table)")
    
    if ( (comment (cb1!!1) cl cb1 cb2 3 3) =="")
        then hPutStr file ""
        else hPutStrLn file ("    (modify-syntax-entry ?"++((cb1!!1):[])++" \". "++(comment (cb1!!1) cl cb1 cb2 3 3) ++  "\" "++nom++"-mode-syntax-table)")
    
    if ( (comment (cb2!!0) cl cb1 cb2 4 4) =="")
        then hPutStr file ""
        else hPutStrLn file ("    (modify-syntax-entry ?"++((cb2!!0):[])++" \". "++(comment (cb2!!0) cl cb1 cb2 4 4) ++  "\" "++nom++"-mode-syntax-table)")
    
    if ( (comment (cb2!!1) cl cb1 cb2 5 5) =="")
        then hPutStr file ""
        else hPutStrLn file ("    (modify-syntax-entry ?"++((cb2!!1):[])++" \". "++(comment (cb2!!1) cl cb1 cb2 5 5) ++  "\" "++nom++"-mode-syntax-table)")
            
commentBlocks file fichier=do
    cl<-(genCommentLine fichier)
    cb<-(genCommentBlock fichier)
    nom<-(genName fichier)
    writeCommentBlock file (head cl) (fst (head cb)) (snd (head cb)) (head nom)

identation file fichier= do
    paire <-(genPaire fichier)
    if (fst(head paire))/=""
        then do 
            hPutStrLn file ("(setq openIdent \"\\\\("++(createRegex (map fst paire))++")\")")
            hPutStrLn file ("(setq closeIdent \"\\\\("++(createRegex (map snd paire))++")\")")
            identfunction<-(openFile "EmacsIdent" ReadMode)
            contents<-hGetContents identfunction
            hPutStrLn file contents
            hClose identfunction
        else
            hPutStr file ""


fin file fichier=do
   nom<-(genName fichier)
   paire<-(genPaire fichier)
   hPutStrLn file ("(setq "++(head nom)++"-font-lock-keywords "++(head nom)++"-font-lock-keywords1)")
   hPutStrLn file ("\n(defun "++(head nom)++"-mode ()\n(interactive)\n(kill-all-local-variables)")--function qui defini le mode
   hPutStrLn file ("(set  (make-local-variable 'font-lock-defaults) '("++(head nom)++"-font-lock-keywords))")
   hPutStrLn file ("(use-local-map "++(head nom)++"-mode-map)")
   hPutStrLn file ("(set-syntax-table "++(head nom)++"-mode-syntax-table)") 
   hPutStrLn file ("(setq major-mode '"++(head nom)++"-mode)")
   hPutStrLn file ("(setq mode-name \""++(head nom)++"\")")
   if (fst(head paire))/= ""
     then  hPutStrLn file " (set (make-local-variable 'indent-line-function) 'my-indent-line) "
     else hPutStr file ""
   hPutStrLn file ("(run-hooks '"++(head nom)++"-mode-hook))")
   hPutStrLn file ("(provide '"++(head nom)++"-mode)")
   hClose file
