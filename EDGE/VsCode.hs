module VsCode where

import System.IO
import System.Directory
import ParseEdge

      
debut file fichier= do
  nom<-(genName fichier)
  hPutStrLn file "{"
  hPutStrLn file "\t\"$schema\": \"https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json\","
  hPutStrLn file ("\t\"name\": \""++(head nom)++"\",")
  hPutStrLn file "\t\"patterns\": ["
  hPutStrLn file "\t\t{"
  hPutStrLn file "\t\t\t\"include\": \"#code\""
  hPutStrLn file "\t\t\n}\t\n],"
  hPutStrLn file "\t\"repository\": {"
  hPutStrLn file "\t\t\"code\":\n  {"
  hPutStrLn file "\t\t\t\"patterns\": [\n    {"
  hPutStrLn file "\t\t\t\t\t\"include\": \"#keywords\""
  hPutStrLn file "\t\t\t\t},\n\t\t\t\t{\n"
  hPutStrLn file "\t\t\t\t\t\"include\": \"#commentLine\"\n\t\t\t\t},\n\t\t\t\t{"
  hPutStrLn file "\t\t\t\t\t\"include\": \"#commentBlock\"\n\t\t\t\t},\n\t\t\t\t{"
  hPutStrLn file "\t\t\t\t\t\"include\": \"#strings\"\n\t\t\t\t},\n\t\t\t\t{"
  hPutStrLn file "\t\t\t\t\t\"include\": \"#constantsChar\"\n\t\t\t\t},\n\t\t\t\t{"
  hPutStrLn file "\t\t\t\t\t\"include\": \"#idents\"\n\t\t\t\t},\n\t\t\t\t{"
  hPutStrLn file "\t\t\t\t\t\"include\": \"#constantsNum\"\n\t\t\t\t},\n\t\t\t\t{"
  hPutStrLn file "\t\t\t\t\t\"include\": \"#operators\"\n\t\t\t\t}\n\t\t\t]\n\t\t},"
          
fin file fichier=do
  nom<-(genName fichier)
  hPutStrLn file ("},\"scopeName\": \"source."++(head nom)++"\"}")

reg (x:[]) sep=regex x
reg (x:xs) sep=(regex x)++sep++ (reg xs sep)

motReserver = "\\*.+^?()|[]{}$"
regexComment []=[]
regexComment (c:r)=if (any (== c) motReserver) then '\\':'\\':c:(regexComment r) else c:(regexComment r)

regex []=[]
regex ('\\':r)='\\':'\\':(regex r)
regex (c:r)=c:(regex r)

keywords file fichier=do
  kw<-(genKeyword fichier)
  nom<-(genName fichier)
  hPutStrLn file "\"keywords\": {"
  hPutStrLn file "\"patterns\": [\n    {"
  hPutStrLn file ("   \"name\": \"keyword.control."++(head nom)++"\",")
  hPutStrLn file ("   \"match\": \"\\\\b("++ (reg kw "|" )++")\\\\b\"\n    }\n  ]\n}")
      
constants file fichier=do
  nom<-(genName fichier)
  cts<-(genConstant fichier)
  hPutStrLn file "\"constantsChar\": {"
  hPutStrLn file "\"patterns\": [\n    {"
  hPutStrLn file ("    \"name\": \"constant.character."++(head nom)++"\",")
  hPutStrLn file ("    \"match\": \"\\\\b("++(reg cts "|")++")\\\\b\"\n    }\n  ]\n}")

operators file fichier= do
  nom<-(genName fichier)
  op<-(genOperateur fichier)
  hPutStrLn file "\"operators\": {"
  hPutStrLn file "\"patterns\": [\n    {"
  hPutStrLn file ("    \"name\": \"keyword.operator."++(head nom)++"\",")
  hPutStrLn file ("    \"match\": \""++(reg op "|")++"\"\n    }\n  ]\n}")
      
idents file fichier=do
  nom<-(genName fichier)
  id<-(genIdent fichier)
  hPutStrLn file "\"idents\": {"
  hPutStrLn file "\"patterns\": [\n    {"
  hPutStrLn file ("    \"name\": \"variable.name."++(head nom)++"\",")
  hPutStrLn file ("    \"match\": \""++(regex (head id ))++ "\"\n    }\n  ]\n}")
            
numbers file fichier=do
  nom<-(genName fichier)
  int<-(genInt fichier)
  hPutStrLn file " \"constantsNum\": {"
  hPutStrLn file "\"patterns\": [\n    {"
  hPutStrLn file ("    \"name\": \"constant.numeric."++(head nom)++"\",")
  hPutStrLn file ("    \"match\": \""++(regex (head int))++"\"\n    }\n  ]\n}")
       
commentLines file fichier=do
  nom<-(genName fichier)
  cl<-(genCommentLine fichier)
  hPutStrLn file "\"commentLine\": {\n"
  hPutStrLn file "\"patterns\": [\n    {\n"
  hPutStrLn file ("    \"name\": \"comment.line."++ (head nom)++"\",\n")
  hPutStrLn file ("    \"match\": \""++(regexComment (head cl))++".*$\"\n    }\n  ]\n}")
      
commentBlocks file fichier=do
  nom<-(genName fichier)
  cb<-(genCommentBlock fichier)
  hPutStrLn file "\"commentBlock\": {"
  hPutStrLn file "\"patterns\": [\n    {"
  hPutStrLn file ("    \"name\": \"comment.block."++(head nom)++"\",")
  hPutStrLn file ("    \"begin\": \""++(regexComment (fst (head cb)))++"\",")
  hPutStrLn file ("    \"end\": \" "++(regexComment (snd (head cb)))++"\"    }\n  ]\n}")

string file fichier =do
    hPutStrLn file ("\"strings\": {")
    hPutStrLn file ("    \"name\": \"string.quoted.double.ilpml\",")
    hPutStrLn file ("   \"begin\": \"\\\"\",")
    hPutStrLn file ("    \"end\": \"\\\"\",")
    hPutStrLn file ("    \"patterns\": [")
    hPutStrLn file ("      {")
    hPutStrLn file ("        \"name\": \"constant.character.escape.ilpml\",")
    hPutStrLn file ("        \"match\": \"\\\\.\"")
    hPutStrLn file ("      }     ]    }")
        
millieu file fichier=do
  nom<-(genName fichier)
  kw<-(genKeyword fichier)
  cts<-(genConstant fichier)
  op<-(genOperateur fichier)
  id<-(genIdent fichier)
  num<-(genInt fichier)
  cl<-(genCommentLine fichier)
  cb<-(genCommentBlock fichier)
  
  string file fichier
  if ((head kw) /="" )
    then hPutStrLn file "," else hPutStr file ""
  if ((head kw)=="") then hPutStr file "" else keywords file fichier
  
  if ( (head cts)/=""  )
    then hPutStrLn file "," else hPutStr file ""
  if ((head cts)=="") then hPutStr file "" else constants file fichier
  
  if ( (head op)/=""  )
    then hPutStrLn file "," else hPutStr file ""
  if ((head op)=="") then hPutStr file "" else operators file fichier

  if ( (head id)/=""  )
    then hPutStrLn file "," else hPutStr file ""
  if ((head id)=="") then hPutStr file "" else idents file fichier
 
  if ( (head num)/=""  )
    then hPutStrLn file "," else hPutStr file ""
  if ((head num)=="") then hPutStr file "" else numbers file fichier

  if ( (head cl)/=""  )
    then hPutStrLn file "," else hPutStr file ""
  if ((head cl)=="") then hPutStr file "" else commentLines file fichier

  if ((fst(head cb))/="") then hPutStrLn file "," else hPutStr file ""
  if ((fst(head cb))=="") then hPutStr file "" else commentBlocks file fichier
 

configComment file fichier =do
  nom<-(genName fichier)
  cl<-(genCommentLine fichier)
  cb<-(genCommentBlock fichier)
  hPutStrLn file "\t\"comments\": {"
  hPutStrLn file "\t\t // symbol used for single line comment. Remove this entry if your language does not support line comments"
  hPutStrLn file ("\t\t\"lineComment\": \""++(head cl)++"\",")
  hPutStrLn file "\t\t// symbols used for start and end a block comment. Remove this entry if your language does not support block comments"
  hPutStrLn file ("\t\t\"blockComment\": [ \""++(regex (fst (head cb)))++"\", \""++(regex (snd (head cb)))++"\" ]\n\t},")

configPeers (x:[])= "\t\t[\""++(regex (fst x)) ++"\",\""++(regex (snd x))++"\"],"
configPeers (x:xs)= "\t\t[\""++(regex (fst x)) ++"\",\""++(regex (snd x))++"\"],\n"++(configPeers xs)

congif file fichier=do
    peers<-(genPaire fichier)
    cb<-(genCommentBlock fichier)
    hPutStrLn file "{" 
    configComment file fichier
    hPutStrLn file "\t// symbols used as brackets"
    hPutStrLn file "\t\"brackets\": ["
    hPutStrLn file (configPeers peers)
    hPutStrLn file "\t],"
    hPutStrLn file "\t// symbols that are auto closed when typing"
    hPutStrLn file "\t\"autoClosingPairs\": ["
    hPutStrLn file (configPeers peers)
    hPutStrLn file "\t\t [\"\\\"\",\"\\\"\"]\n\t],"
    hPutStrLn file "\t// symbols that that can be used to surround a selection"
    hPutStrLn file "\t\"surroundingPairs\": ["
    hPutStrLn file (configPeers peers)
    if ((fst(head cb))/="") 
      then    hPutStrLn file (configPeers cb) else hPutStr file ""

    hPutStrLn file "\t\t[\"\\\"\",\"\\\"\"]"
    hPutStrLn file "\t]"
    hPutStrLn file "\n}"

package file fichier=do
    nom<-(genName fichier)
    ext<-(genExtension fichier)
    hPutStrLn file ("{  \"name\": \""++(head nom)++"\",")
    hPutStrLn file ("  \"displayName\": \""++(head nom)++"\",")
    hPutStrLn file ("  \"description\": \"Fournit la coloration syntaxique pour les fichiers "++(head nom)++"\",")
    hPutStrLn file ("  \"version\": \"0.0.1\",")
    hPutStrLn file ("  \"publisher\": \"Blank\",")
    hPutStrLn file ("  \"engines\": {")
    hPutStrLn file ("      \"vscode\": \"^1.21.0\"\n  },")
    hPutStrLn file ("  \"categories\": [\n      \"Languages\"\n  ],")
    hPutStrLn file ("  \"contributes\": {")
    hPutStrLn file ("     \"languages\": [{")
    hPutStrLn file ("          \"id\": \""++(head nom)++"\",")
    hPutStrLn file ("          \"aliases\": [\""++(head nom)++"\"],")
    hPutStrLn file ("           \"extensions\": [\"."++(head ext)++"\"],")
    hPutStrLn file ("        \"configuration\": \"./language-configuration.json\"\n        }],")
    hPutStrLn file ("    \"grammars\": [{")
    hPutStrLn file ("        \"language\": \""++(head nom)++"\",")
    hPutStrLn file ("        \"scopeName\": \"source."++(head nom)++"\",")
    hPutStrLn file ("      \"path\": \"./syntaxes/"++(head nom)++".tmLanguage.json\"\n       }]\n    }\n}")



vsCodeExtension fichier=do
  nom<-(genName fichier)
  createDirectory "vsCode"
  createDirectory ("vsCode/"++(head nom))
  createDirectory ("vsCode/"++(head nom)++"/syntaxes")
  fichierCongif <- openFile ("vsCode/"++(head nom)++"/language-configuration.json") WriteMode
  syntaxe <- openFile ("vsCode/"++(head nom)++"/syntaxes/"++(head nom)++".tmLanguage.json") WriteMode
  packagefile <- openFile ("vsCode/"++(head nom)++"/package.json")  WriteMode
  debut syntaxe fichier
  millieu syntaxe fichier
  fin syntaxe fichier
  hClose syntaxe 
  congif fichierCongif fichier
  hClose fichierCongif
  package packagefile fichier
  hClose packagefile



