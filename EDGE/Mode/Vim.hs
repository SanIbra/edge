module Mode.Vim where
import Mode.VimIndent
import Parseur.TypeStats
import FileGenerator
import Parseur.Regex
     
vimgene:: Stats -> [ToGenerate]
vimgene stats = 
  [
    Folders [
      "vim",
      "vim/"++ name ++".vim",
      "vim/"++ name ++".vim/colors",
      "vim/"++ name ++".vim/compiler",
      "vim/"++ name ++".vim/doc",
      "vim/"++ name ++".vim/ftdetect",
      "vim/"++ name ++".vim/ftplugin",
      "vim/"++ name ++".vim/indent",
      "vim/"++ name ++".vim/keymap",
      "vim/"++ name ++".vim/plugin",
      "vim/"++ name ++".vim/syntax"
    ],
    File ("vim/"++ name ++".vim/ftdetect/"++ name ++ ".vim", "au BufRead,BufNewFile *"++ ext ++" set filetype="++ name),
    File ("vim/"++ name ++".vim/syntax/"++ name ++".vim", (syntax stats)++(syntaxfin stats)),
    File ("vim/"++ name ++".vim/indent/"++ name ++ ".vim", writeIndent stats)
  ]
  where
    name = head (getName stats)
    ext = head (getExtension stats)

syntax::Stats->String
syntax stats = 
  kw
  ++ cts
  ++ op
  ++ id
  ++ num
  ++ cl
  ++ cb
  ++ vimString stats
  where
    kw = if (head (getKeyword stats)) == "" then "" else vimKeywords stats
    cts = if (head (getConstant stats)) == "" then "" else vimConstants stats
    op = if (head (getOperateur stats)) == "" then "" else vimOperators stats
    id = if (head (getIdent stats)) == "" then "" else vimIdents stats
    num = if (head (getInt stats)) == "" then "" else vimNumbers stats
    cl = if (head (getCommentLine stats)) == "" then "" else vimCommentLines stats
    cb = if (fst (head (getCommentBlock stats))) == "" then "" else vimCommentBlocks stats
    


syntaxfin::Stats->String
syntaxfin stats =
  "let b:current_syntax = \""++ name ++"\"\n"
  ++ kw
  ++ cl
  ++ cb
  ++ cts
  ++ id
  ++ num
  ++ op
  ++ "hi def link "++ name ++"String  String\n"
  ++ cbl
  where
    name = head (getName stats)
    kw = if (head (getKeyword stats)) == "" then "" else "hi def link "++ name ++"Keyword  Keyword\n"
    cts = if (head (getConstant stats)) == "" then "" else "hi def link "++ name ++"Const  Constant\n"
    op = if (head (getOperateur stats)) == "" then "" else "hi def link "++ name ++"Operator  Operator\n"
    id = if (head (getIdent stats)) == "" then "" else "hi def link "++ name ++"Ident  Identifier\n"
    num = if (head (getInt stats)) == "" then "" else "hi def link "++ name ++"Int   Number\n"
    cl = if (head (getCommentLine stats)) == "" then "" else "hi def link "++ name ++"CommentLine  Comment\n"
    cb = if (fst (head (getCommentBlock stats))) == "" then "" else "hi def link "++ name ++"CommentBlock  Comment\n"
    cbl = if (fst (head (getCommentBlock stats))) == "" then "" else "hi def link "++ name ++"CommentBlockLevel  Comment\n"

  
vimKeywords::Stats->String
vimKeywords stats = "syn keyword "++ name ++"Keyword "++ kw ++ "\n"
  where
    name = head (getName stats)
    kw = vimprint (getKeyword stats) " "

vimConstants::Stats->String
vimConstants stats = "syn keyword  "++ name ++"Const   "++ cts ++ "\n"
  where
    name = head (getName stats)
    cts = vimprint (getConstant stats) " "


vimOperators  ::Stats->String
vimOperators stats = "syn match "++ name ++"Operator display \""++ op ++"\"\n"
  where
    name = head (getName stats)
    op = vimprint (getOperateur stats) "|"

vimIdents::Stats->String
vimIdents stats = "syn match "++ name ++"Ident display \""++ id ++"\"\n"
  where
    name = head (getName stats)
    id = toVim (RE (head (getIdent stats)))

vimNumbers::Stats->String
vimNumbers stats = "syn match "++ name ++"Int display \""++ int ++"\"\n"
  where
    name = head (getName stats)
    int = toVim (RE (head (getInt stats)))

vimCommentLines  ::Stats->String
vimCommentLines stats = "syn region "++ name ++"CommentLine start=\""++ cl ++"\" end=\"$\"\n"
  where
    name = head (getName stats)
    cl = head (getCommentLine stats)

vimCommentBlocks ::Stats->String
vimCommentBlocks stats = "syn region "++ name ++"CommentBlock matchgroup="++ name ++"CommentBlock start=\""++ cb1 ++"\" end=\""++ cb2 ++"\" fold contains="++ name ++"CommentBlockLevel\n"
  ++ "syn region "++ name ++"CommentBlockLevel matchgroup="++ name ++"CommentBlock start=\""++ cb1 ++"\" end=\""++ cb2 ++"\" contains="++ name ++"CommentBlockLevel contained\n"
  where
    name = head (getName stats)
    cb1 = toVim (fromStringtoReg (fst (head (getCommentBlock stats))))
    cb2 = toVim (fromStringtoReg (snd (head (getCommentBlock stats))))
  
vimString::Stats->String
vimString stats = "syn match "++ name ++"Escape display contained /\\./\n"
  ++ "syn region "++ name ++"String start=+\"+ end=+\"+ contains="++ name ++"Escape\n"
  where
    name = head (getName stats)

vimprint:: [String]->String->String
vimprint (x:[]) sep=x
vimprint (x:xs) sep=x++sep++(vimprint xs sep)