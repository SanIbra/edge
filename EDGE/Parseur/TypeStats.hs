module Parseur.TypeStats where

-- DEBUT TYPE STATS --

data Stats = Stats { name :: [String]
       , extension :: [String]
       , keyword :: [String]
       , constant :: [String]
       , operateur :: [String]
       , ident :: [String]
       , int :: [String]
       , paire :: [(String,String)]
       , commentLine :: [String]
       , commentBlock :: [(String,String)]
} deriving (Show)

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

getIdent :: Stats -> [String]
getIdent st = id where Stats _ _ _ _ _ id _ _ _ _ = st

getInt :: Stats -> [String]
getInt st = int where Stats _ _ _ _ _ _ int _ _ _ = st

getPaire :: Stats -> [(String,String)]
getPaire st = p where Stats _ _ _ _ _ _ _ p _ _ = st

getCommentLine :: Stats -> [String]
getCommentLine st = cl where Stats _ _ _ _ _ _ _ _ cl _ = st

getCommentBlock :: Stats -> [(String,String)]
getCommentBlock st = cb where Stats _ _ _ _ _ _ _ _ _ cb = st

-- DEFAULT --
dname = [""]
dext = [""]
dkw = [""]
dcst = [""]
dop = [""]
dident = [""]
dint = [""]
dpaire = [("","")]
dcommentLine = [""]
dcommentBlock = [("","")]

def = Stats dname dext dkw dcst dop dident dint dpaire dcommentLine dcommentBlock

-- FIN TYPE STATS + VALEURS PAR DEFAUT --