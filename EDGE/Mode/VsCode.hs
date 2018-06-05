module Mode.VsCode where

import Parseur.TypeStats
import Parseur.Regex
import Generateur.FileGenerator
      
-- beginning of the syntax file
debutVSC :: Stats -> String
debutVSC stats = "{\n"
  ++ "\t\"$schema\": \"https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json\",\n"
  ++ ("\t\"name\": \""++ name ++"\",")
  ++ "\t\"patterns\": [\n"
  ++ "\t\t{\n"
  ++ "\t\t\t\"include\": \"#code\"\n"
  ++ "\t\t}\n\t],\n"
  ++ "\t\"repository\": {\n"
  ++ "\t\t\"code\":\n\t\t{\n"
  ++ "\t\t\t\"$comment\": \"the patterns attribut of code include all of the syntax objects\",\n"
  ++ "\t\t\t\"patterns\": [\n\t\t\t\t{\n"
  ++ "\t\t\t\t\t\"include\": \"#keywords\"\n"
  ++ "\t\t\t\t},\n\t\t\t\t{\n"
  ++ "\t\t\t\t\t\"include\": \"#commentLine\"\n\t\t\t\t},\n\t\t\t\t{\n"
  ++ "\t\t\t\t\t\"include\": \"#commentBlock\"\n\t\t\t\t},\n\t\t\t\t{\n"
  ++ "\t\t\t\t\t\"include\": \"#strings\"\n\t\t\t\t},\n\t\t\t\t{\n"
  ++ "\t\t\t\t\t\"include\": \"#constantsChar\"\n\t\t\t\t},\n\t\t\t\t{\n"
  ++ "\t\t\t\t\t\"include\": \"#idents\"\n\t\t\t\t},\n\t\t\t\t{\n"
  ++ "\t\t\t\t\t\"include\": \"#constantsNum\"\n\t\t\t\t},\n\t\t\t\t{\n"
  ++ "\t\t\t\t\t\"include\": \"#operators\"\n\t\t\t\t}\n\t\t\t]\n\t\t},\n"
  where name = head (getName stats)

-- last line of the syntax file
finVSC :: Stats -> String
finVSC stats = "},\"scopeName\": \"source."++ name ++"\"}\n" where name = head (getName stats)

-- transform a String to its equivalent in a Regex in VSCode
regex::String->String
regex x = toVSCode (fromStringtoReg (fromSymboltoString x))

-- call regex on each string in the list in parameter and put sep between them
reg::[String]->String->String
reg (x:[]) sep=regex x
reg (x:xs) sep=(regex x)++sep++(reg xs sep)

-- return the keywords JSON object
keywords :: Stats -> String
keywords stats = 
  "\"keywords\": {\n"
  ++ "\"patterns\": [\n    {\n"
  ++ "   \"name\": \"keyword.control."++ name ++"\",\n"
  ++ "   \"match\": \"\\\\b("++ kw ++")\\\\b\"\n    }\n  ]\n}\n"
  where 
    name = head (getName stats)
    kw = reg (getKeyword stats) "|"

-- return the constants JSON object
constants::Stats -> String
constants stats= "\"constantsChar\": {\n"
  ++ "\"patterns\": [\n    {\n"
  ++ ("    \"name\": \"constant.character."++ name ++"\",")
  ++ ("    \"match\": \"\\\\b("++ cts ++")\\\\b\"\n    }\n  ]\n}")
  where 
    name = head (getName stats)
    cts = reg (getConstant stats) "|"

-- return the operators JSON object
operators:: Stats -> String
operators stats= "\"operators\": {\n"
  ++ "\"patterns\": [\n    {\n"
  ++ "    \"name\": \"keyword.operator."++ name ++"\",\n"
  ++ "    \"match\": \""++ op ++"\"\n    }\n  ]\n}\n"
  where 
    name = head (getName stats)
    op = reg (getOperateur stats) "|"
     
-- return the idents JSON object 
idents:: Stats -> String
idents stats= "\"idents\": {\n"
  ++ "\"patterns\": [\n    {\n"
  ++ "    \"name\": \"variable.name."++ name ++"\",\n"
  ++ "    \"match\": \""++ id ++ "\"\n    }\n  ]\n}\n"
  where
    name = head (getName stats)
    id = toVSCode (head (getIdent stats))


-- return the constantsNum JSON object
numbers::Stats -> String
numbers stats= " \"constantsNum\": {\n"
  ++ "\"patterns\": [\n    {\n"
  ++ "    \"name\": \"constant.numeric."++ name ++"\",\n"
  ++ "    \"match\": \""++ int ++"\"\n    }\n  ]\n}\n"
  where
    name = head (getName stats)
    int = toVSCode (head (getInt stats))
     
-- return the commentLine JSON object  
commentLines::Stats -> String
commentLines stats= "\"commentLine\": {\n"
  ++ "\"patterns\": [\n    {\n"
  ++      "    \"name\": \"comment.line."++ name ++"\",\n"
  ++ "    \"match\": \""++ cl ++".*$\"\n    }\n  ]\n}\n"
  where
    name = head (getName stats)
    cl = toVSCode (fromStringtoReg (head (getCommentLine stats)))
     
-- return the commentBlock JSON object
commentBlocks::Stats -> String
commentBlocks stats= "\"commentBlock\": {\n"
  ++ "\"patterns\": [\n    {\n"
  ++ "    \"name\": \"comment.block."++ name ++"\",\n"
  ++ "    \"begin\": \""++ cb1 ++"\",\n"
  ++ "    \"end\": \" "++ cb2 ++"\"    }\n  ]\n}\n"
  where
    name = head (getName stats)
    cb1 = toVSCode (fromStringtoReg (fst (head (getCommentBlock stats))))
    cb2 = toVSCode (fromStringtoReg (snd (head (getCommentBlock stats))))

-- return the strings JSON object (always the same)
string:: String
string = "\"strings\": {\n"
  ++ "    \"name\": \"string.quoted.double.ilpml\",\n"
  ++ "   \"begin\": \"\\\"\",\n"
  ++ "    \"end\": \"\\\"\",\n"
  ++ "    \"patterns\": [\n"
  ++ "      {\n"
  ++ "        \"name\": \"constant.character.escape.ilpml\",\n"
  ++ "        \"match\": \"\\\\.\"\n"
  ++ "      }     ]    }\n"
        
-- middle of the syntax file
millieuVSC ::Stats -> String
millieuVSC stats = 
  string
  ++ kw
  ++ cts
  ++ op
  ++ id
  ++ int
  ++ cl
  ++ cb
  where
    kw = if (head (getKeyword stats)) /= "" then ","++(keywords stats) else ""
    cts = if (head (getConstant stats)) /= "" then ","++(constants stats) else ""
    op = if (head (getOperateur stats)) /= "" then ","++(operators stats) else ""
    id = if (toVSCode (head (getIdent stats))) /= "" then ","++(idents stats) else ""
    int = if (toVSCode (head (getInt stats))) /= "" then ","++(numbers stats) else ""
    cl = if (head (getCommentLine stats)) /= "" then ","++(commentLines stats) else ""
    cb = if (fst (head (getCommentBlock stats))) /= "" then ","++(commentBlocks stats) else ""
 
-- Declaration of the Comments patterns in the language-configuration.json file
configComment::Stats -> String
configComment stats = 
  "\t\"comments\": {\n"
  ++ "\t\t // symbol used for single line comment. Remove this entry if your language does not support line comments\n"
  ++ "\t\t\"lineComment\": \""++ cl ++"\",\n"
  ++ "\t\t// symbols used for start and end a block comment. Remove this entry if your language does not support block comments\n"
  ++ "\t\t\"blockComment\": [ \""++ cb1 ++"\", \""++ cb2 ++"\" ]\n\t},\n"
  where
    cl = head (getCommentLine stats)
    cb1 = fst (head (getCommentBlock stats))
    cb2 = snd (head (getCommentBlock stats))

-- return a string representing the peers in a list
configPeers::[(String,String)]->String
configPeers (x:[])= "\t\t[\""++ fromSymboltoString (fst x) ++"\",\""++ fromSymboltoString (snd x) ++"\"],\n"
configPeers (x:xs)= "\t\t[\""++ fromSymboltoString (fst x) ++"\",\""++ fromSymboltoString (snd x) ++"\"],\n"++(configPeers xs)

-- contents of the language-configuration.json file
congif::Stats -> String
congif stats= "{" 
  ++ configComment stats
  ++ "\t// symbols used as brackets\n"
  ++ "\t\"brackets\": [\n"
  ++ peers
  ++ "\t],\n"
  ++ "\t// symbols that are auto closed when typing\n"
  ++ "\t\"autoClosingPairs\": [\n"
  ++ peers
  ++ "\t\t [\"\\\"\",\"\\\"\"]\n\t],\n"
  ++ "\t// symbols that that can be used to surround a selection\n"
  ++ "\t\"surroundingPairs\": [\n"
  ++ peers
  ++ cb
  ++ "\t\t[\"\\\"\",\"\\\"\"]\n"
  ++ "\t]\n"
  ++ "\n}\n"
  where
    peers = configPeers (getPaire stats)
    cb = if (fst(head (getCommentBlock stats))) /= "" then configPeers (getCommentBlock stats) else "" 

-- contents of the package.json file
package::Stats -> String
package stats= "{\n  \"name\": \""++ name ++"\",\n"
    ++ "  \"displayName\": \""++ name ++"\",\n"
    ++ "  \"description\": \"Fournit la coloration syntaxique pour les fichiers "++ name ++"\",\n"
    ++ "  \"version\": \"0.0.1\",\n"
    ++ "  \"publisher\": \"Blank\",\n"
    ++ "  \"engines\": {\n"
    ++ "      \"vscode\": \"^1.21.0\"\n  },\n"
    ++ "  \"categories\": [\n      \"Languages\"\n  ],\n"
    ++ "  \"contributes\": {\n"
    ++ "     \"languages\": [{\n"
    ++ "          \"id\": \""++ name ++"\",\n"
    ++ "          \"aliases\": [\""++ name ++"\"],\n"
    ++ "           \"extensions\": [\"."++ ext ++"\"],\n"
    ++ "        \"configuration\": \"./language-configuration.json\"\n        }],\n"
    ++ "    \"grammars\": [{\n"
    ++ "        \"language\": \""++ name ++"\",\n"
    ++ "        \"scopeName\": \"source."++ name ++"\",\n"
    ++ "      \"path\": \"./syntaxs/"++ name ++".tmLanguage.json\"\n       }]\n    }\n}\n"
    where
      name = head (getName stats)
      ext = head (getExtension stats)

-- main function (function called)
vsCodeExtension::Stats -> [ToGenerate]
vsCodeExtension stats=
  [
    Folders [
      "vsCode",
      "vsCode/"++ name,
      "vsCode/"++ name ++"/syntaxs"
    ],
    File ("vsCode/"++ name ++"/language-configuration.json", congif stats),
    File ("vsCode/"++ name ++"/syntaxs/"++ name ++".tmLanguage.json", (debutVSC stats) ++ (millieuVSC stats) ++ (finVSC stats)),
    File ("vsCode/"++ name ++"/package.json", package stats)
  ]
  where
    name = head (getName stats)



