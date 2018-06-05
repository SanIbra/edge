module Parseur.TypeStats where
import Parseur.Regex

-- DEBUT TYPE STATS --

data Stats = Stats { name :: [String]
       , extension :: [String]
       , keyword :: [String]
       , constant :: [String]
       , operateur :: [String]
       , ident :: [Regex]
       , int :: [Regex]
       , paire :: [(String,String)]
       , commentLine :: [String]
       , commentBlock :: [(String,String)]
} deriving (Show)

-- Return a Stats with all attributes unchanged except one (one function per attribute)
getName :: Stats -> [String]
getName st = n where Stats n _ _ _ _ _ _ _ _ _ = st

getExtension :: Stats -> [String]
getExtension st = e where Stats _ e _ _ _ _ _ _ _ _ = st

getKeyword :: Stats -> [String]
getKeyword st = k where Stats _ _ k _ _ _ _ _ _ _ = st

getConstant :: Stats -> [String]
getConstant st = c where Stats _ _ _ c _ _ _ _ _ _ = st

getOperateur :: Stats -> [String]
getOperateur st = o where Stats _ _ _ _ o _ _ _ _ _ = st

getIdent :: Stats -> [Regex]
getIdent st = id where Stats _ _ _ _ _ id _ _ _ _ = st

getInt :: Stats -> [Regex]
getInt st = int where Stats _ _ _ _ _ _ int _ _ _ = st

getPaire :: Stats -> [(String,String)]
getPaire st = p where Stats _ _ _ _ _ _ _ p _ _ = st

getCommentLine :: Stats -> [String]
getCommentLine st = cl where Stats _ _ _ _ _ _ _ _ cl _ = st

getCommentBlock :: Stats -> [(String,String)]
getCommentBlock st = cb where Stats _ _ _ _ _ _ _ _ _ cb = st

-- DEFAULT --

-- Default Stats for C language
dCName = ["C"]
dCExt = [".c"]
dCKw = ["function","return","int","bool","string","char","include","if","then",
    "else","for","while","do","break","switch","case","default","const","continue",
    "double","enum","extern","float","goto","long","register","short","signed",
    "sizeof","static","void","struct","typedef","union","unsigned","volatile"]
dCCst = ["true","false"]
dCOp = ["+","*","-","/","=","<",">","!","%","->","~","&","^","|","?",":","."]
dCIdent = [RE "[a-zA-Z][a-zA-Z0-9_]*"]
dCInt = [RE "[0-9]?\\.?[0-9]+"]
dCPaire = [("{","}"),("(",")"),("[","]")]
dCCommentLine = ["//"]
dCCommentBlock = [("/*","*/")]

defC = Stats dCName dCExt dCKw dCCst dCOp dCIdent dCInt dCPaire dCCommentLine dCCommentBlock


-- Default Stats for Ocaml language
dCamName = ["Ocaml"]
dCamExt = [".ml"]
dCamKw = ["let","rec","in","int","bool","string","char","list","if","then","else","for","while",
    "do","function","fun","of","prefix","type","and","match","with","unit","ref","as","assert",
    "begin","class","constraint","end","exception","external","functor","include","inherit",
    "initializer","lazy","method","module","mutable","new","object","or","open","sig","try","to",
    "val","virtual","when","not"]
dCamCst = ["true","false"]
dCamOp = ["+","*","-","/","=","<",">","&&","||"]
dCamIdent = [RE "[a-zA-Z][a-zA-Z0-9_]*"]
dCamInt = [RE "[0-9]?\\.?[0-9]+"]
dCamPaire = [("(",")"),("[","]")]
dCamCommentLine = [""]
dCamCommentBlock = [("(*","*)")]

defCam = Stats dCamName dCamExt dCamKw dCamCst dCamOp dCamIdent dCamInt dCamPaire dCamCommentLine dCamCommentBlock


-- Default Stats for Python language
dPyName = ["Python"]
dPyExt = [".py"]
dPyKw = ["or","and","not","is","in","as","assert","break","class","continue","def","del","elif",
    "else","except","exec","finally","for","from","global","if","import","lambda","pass","print",
    "raise","return","sort","try","while","yield","int","long","int","float","complex","str",
    "unicode","tuple","list","dict","file","bool","NoneType","NotImplementedType","function",
    "module"]
dPyCst = ["True","False","None"]
dPyOp = ["<","=",">","!=","|","^","&","+","-","*","/","%","~","."]
dPyIdent = [RE "[a-zA-Z][a-zA-Z0-9_]*"]
dPyInt = [RE "[0-9]?\\.?[0-9]+"]
dPyPaire = [("{","}"),("(",")"),("[","]")]
dPyCommentLine = ["#"]
dPyCommentBlock = [("\"\"\"","\"\"\"")]

defPy = Stats dPyName dPyExt dPyKw dPyCst dPyOp dPyIdent dPyInt dPyPaire dPyCommentLine dPyCommentBlock

