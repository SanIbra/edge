module Mode.Emacs where
import Parseur.TypeStats
import Mode.EmacsIndent
import FileGenerator

emacskeyword key= "\\\\<\\\\("++(createRegex key)++"\\\\)\\\\>"
createRegex (k:[]) =k
createRegex (k:ks)=k++"\\\\|"++createRegex ks


emacsMode::Stats->[ToGenerate]
emacsMode stat =
    (File (filename,contents)):[]
  where filename=((head (getName stat))++".el")
        contents=headFile stat
         ++codeFolding stat
         ++coloration stat
         ++commentSyntax stat
         ++indentation stat
         ++keymapping stat
         ++fin stat
    
headFile::Stats->String
headFile stat=
    ";;Commande a ajouter dans le .emacs pour que le mode se charge automatiquement à l'ouverture de emacs\n"
      ++";;(autoload '"++nom++"-mode \"chemin du mode.el\" \""++nom++" Mode editing\" t\n)"
      ++";;(add-to-list 'auto-mode-alist '(\"\\\\"++ext++"\\\\'\" . "++nom++"-mode)) \n" 
  where nom=head(getName stat)
        ext=head(getExtension stat)

codeFolding::Stats->String
codeFolding stat=
      if (fst(head paire))/=""
        then 
            ""
        else
            ";Pour le code folding utilisation du outline mode"
            ++"(require 'outline)\n(setq outline-regex openIdent)\n\n"
    where paire=(getPaire stat)

coloration::Stats->String
coloration stat =
    "\n;Responsable de la coloration syntaxique\n"
      ++"(defconst "++nom++"-font-lock-keywords1(list\n"--description des keywords\n"
        ++ if ((head kw)=="") then "" else  ("  '(\""++(emacskeyword kw)++"\". font-lock-keyword-name-face)\n")
        ++ if ((head cts)=="") then "" else  ("  '(\""++(emacskeyword cts)++"\". font-lock-constant-face)\n")
        ++ if ((head id)=="") then "" else ("  '(\""++(head id)++"\". font-lock-variable-name-face)\n")
        ++ "))"
        ++ "(setq "++nom++"-font-lock-keywords "++nom++"-font-lock-keywords1)"
  where nom=head (getName stat)
        kw=(getKeyword stat)
        cts=(getConstant stat)
        id=(getIdent stat)


keymapping::Stats->String
keymapping stat=
    ";Permet de faire le keyBinding \n"
      ++"(defvar "++nom++"-mode-map\n"
      ++"  (let (("++nom++"-mode-map (make-keymap))))\n"
      ++";    (define-key "++nom++"-mode-map \"\\C-j\" 'newline-and-indent) la commande CTRL+J va appliquer la fonction newligne-and-indent)\n"
      ++nom++"-mode-map)\n"
    where nom=head(getName stat)

-----------------------
--Ecriture des commmentaires 
traiter::String->Char->Bool
traiter [] el = False
traiter (x:xs) el =  if el==x 
                        then True else traiter xs el

--forme un string des n premier element de s
couperString::String->Int->String
couperString s 0=[]
couperString [] n=[]
couperString (x:xs) n=x:(couperString xs (n-1))

comL::Char->String->Int->String
comL _ [] _=[]
comL c tt 0=if c==(tt!!0) then '1':(comL c tt 1) else (comL c tt 1)
comL c tt 1=if c==(tt!!1) then '2':(comL c tt 2) else (comL c tt 2)
comL c tt 2=if ( (c==(tt!!2)) && not (c==(tt!!0)) ) then '1':(comL c tt 3) else (comL c tt 3)
comL c tt 3=if ( (c==(tt!!3))  && not (c==(tt!!1)) ) then '2':(comL c tt 4) else (comL c tt 4)
comL c tt 4=if c==(tt!!4) then '3':(comL c tt 5) else (comL c tt 5)
comL c tt 5=if c==(tt!!5) then '4':[] else []

commentSyntax::Stats->String
commentSyntax stat=
    "(defvar "++nom++"-mode-syntax-table\n"
        ++"  (let (("++nom++"-mode-syntax-table (make-syntax-table)))\n"
        ++ (if (cl=="")  then  "" else (commentLines stat)++"\n")
        ++ (if ((fst(head cb))=="") then "" else (commentBlocks stat)++"\n")
        ++"  "++nom++"-mode-syntax-table)\n"
        ++ "\"Syntax table for "++nom++"-mode\")\n"
  where nom=head((getName stat))
        cb=(getCommentBlock stat)
        cl=head((getCommentLine stat)) 

comment::Char->String->String->String->Int->String
comment c cl cb1 cb2 n=if (traiter (couperString (cl++cb1++cb2) n) c) then "" else (comL c (cl++cb1++cb2) 0)

commentLines::Stats->String
commentLines stat=
    (if ( (comment (cl!!0) cl (fst cb) (snd cb) 1) =="")
        then ""
        else ("    (modify-syntax-entry ?"++((cl!!0):[])++" \". "++(comment (cl!!0) cl (fst cb) (snd cb) 1)++  " \" "++nom++"-mode-syntax-table)"))
      ++ ("    (modify-syntax-entry ?"++((cl!!1):[])++" \". "++(comment (cl!!1) cl (fst cb) (snd cb) 0)++  "b\" "++nom++"-mode-syntax-table)")
      ++ ("    (modify-syntax-entry ?\\n \"> b\" "++nom++"-mode-syntax-table)")
  where nom=head(getName stat)
        cl=head(getCommentLine stat)
        cb=head(getCommentBlock stat)
writeCommentBlock::String->String->String->String->String
writeCommentBlock cl cb1 cb2 nom=do
    (if ( (comment (cb1!!0) cl cb1 cb2 2) =="")
        then ""
        else ("    (modify-syntax-entry ?"++((cb1!!0):[])++" \". "++(comment (cb1!!0) cl cb1 cb2 2) ++  "\" "++nom++"-mode-syntax-table)\n"))
    
        ++
        (if ( (comment (cb1!!1) cl cb1 cb2 3) =="")
            then ""
            else ("    (modify-syntax-entry ?"++((cb1!!1):[])++" \". "++(comment (cb1!!1) cl cb1 cb2 3) ++  "\" "++nom++"-mode-syntax-table)\n"))
        ++
        (if ( (comment (cb2!!0) cl cb1 cb2 4) =="")
            then ""
            else ("    (modify-syntax-entry ?"++((cb2!!0):[])++" \". "++(comment (cb2!!0) cl cb1 cb2 4) ++  "\" "++nom++"-mode-syntax-table)\n"))
        ++
        (if ( (comment (cb2!!1) cl cb1 cb2 5) =="")
            then ""
            else ("    (modify-syntax-entry ?"++((cb2!!1):[])++" \". "++(comment (cb2!!1) cl cb1 cb2 5) ++  "\" "++nom++"-mode-syntax-table)\n"))
            
commentBlocks::Stats->String
commentBlocks stat=
    writeCommentBlock cl (fst cb) (snd cb) nom
  where cl=head((getCommentLine stat))
        cb=head((getCommentBlock stat))
        nom=head((getName stat))








--------------
indentation::Stats->String
indentation stat=
    if (fst(head paire))/=""
        then do 
        ( ("(setq openIdent \"\\\\("++(createRegex (map fst paire))++")\")")
         ++ ("(setq closeIdent \"\\\\("++(createRegex (map snd paire))++")\")")
         ++ indentfunction)
        else
         ""
  where paire =(getPaire stat)

fin::Stats->String
fin stat=
    "(setq "++nom++"-font-lock-keywords "++nom++"-font-lock-keywords1)"
      ++"\n(defun "++nom++"-mode ()\n(interactive)\n(kill-all-local-variables)"
      ++"(set  (make-local-variable 'font-lock-defaults) '("++nom++"-font-lock-keywords))"
      ++"(use-local-map "++nom++"-mode-map)"
      ++"(set-syntax-table "++nom++"-mode-syntax-table)" 
      ++"(setq major-mode '"++nom++"-mode)"
      ++"(setq mode-name \""++nom++"\")"
      ++(if (fst(head paire))/= ""
          then  " (set (make-local-variable 'indent-line-function) 'my-indent-line) "
          else "")
      ++"(run-hooks '"++nom++"-mode-hook))"
      ++"(provide '"++nom++"-mode)"
    where nom=head (getName stat)
          paire=(getPaire stat)
