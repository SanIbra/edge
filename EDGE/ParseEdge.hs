module ParseEdge where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Char
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.IORef

-- description du langage
-- a ::= extension | keywords | constants | operators

-- b ::= b , b | x

-- c ::= peers

-- d ::= d , d | x x

-- u ::= name : x | idents : x | numbers : x | commentLine : x | commentBlock : x x

-- S ::= S1; S2 | a : b | c : d | u

-- création des structures de données
data UExpr = UExpression String deriving (Show) 

data BExpr = BExpression String String deriving (Show) 

data Stmt = Seq [Stmt]
          | Peers [BExpr]
          | Extension [UExpr]
          | Keywords [UExpr]
          | Constants [UExpr]
          | Operators [UExpr]
          | Name String 
          | Idents String 
          | Numbers String 
          | CommentLine String 
          | CommentBlock String String
            deriving (Show)

-- Utilitaire
forIdent c =
    if c == ' ' || c == '\t' || c == '\n' || c == '\r' then False else True

-- definition du langage
languageDef =
  emptyDef{ Token.commentStart = ""
          , Token.commentEnd = ""
          , Token.commentLine = ""
          , Token.nestedComments = False
          , Token.identStart = char '#'
          , Token.identLetter = satisfy forIdent
          , Token.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
          , Token.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
          , Token.reservedNames = [ "name"
                                  , "extension"
                                  , "keywords"
                                  , "constants"
                                  , "operators"
                                  , "idents"
                                  , "numbers"
                                  , "commentLine"
                                  , "peers"
                                  , "commentBlock"
                                  ]
          , Token.reservedOpNames = [":",","]
          , Token.caseSensitive = True
         }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
comma      = Token.comma      lexer -- parses a comma
whiteSpace = Token.whiteSpace lexer -- parses whitespace

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement = sequenceOfStmt

sequenceOfStmt =
  do list <- (sepBy1 statement' semi)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' = nameStmt
        <|> extensionStmt
        <|> keywordsStmt
        <|> constantsStmt
        <|> operatorsStmt
        <|> identsStmt
        <|> numbersStmt
        <|> commentLineStmt
        <|> peersStmt
        <|> commentBlockStmt

nameStmt :: Parser Stmt
nameStmt =
    do reserved "name"
       reservedOp ":"
       n <- identifier
       return $ Name n

extensionStmt :: Parser Stmt
extensionStmt =
    do reserved "extension"
       reservedOp ":"
       cl <- (sepBy1 lExpression comma)
       return $ Extension cl

keywordsStmt :: Parser Stmt
keywordsStmt =
    do reserved "keywords"
       reservedOp ":"
       cl <- (sepBy1 lExpression comma)
       return $ Keywords cl
        
constantsStmt :: Parser Stmt
constantsStmt =
    do reserved "constants"
       reservedOp ":"
       cl <- (sepBy1 lExpression comma)
       return $ Constants cl

operatorsStmt :: Parser Stmt
operatorsStmt =
    do reserved "operators"
       reservedOp ":"
       cl <- (sepBy1 lExpression comma)
       return $ Operators cl

identsStmt :: Parser Stmt
identsStmt =
    do reserved "idents"
       reservedOp ":"
       i <- identifier
       return $ Idents i

numbersStmt :: Parser Stmt
numbersStmt =
    do reserved "numbers"
       reservedOp ":"
       n <- identifier
       return $ Numbers n

commentLineStmt :: Parser Stmt
commentLineStmt =
    do reserved "commentLine"
       reservedOp ":"
       cl <- identifier
       return $ CommentLine cl

peersStmt :: Parser Stmt
peersStmt =
    do reserved "peers"
       reservedOp ":"
       cl <- (sepBy1 pExpression comma)
       return $ Peers cl

commentBlockStmt :: Parser Stmt
commentBlockStmt =
    do reserved "commentBlock"
       reservedOp ":"
       s <- identifier
       e <- identifier
       return $ CommentBlock s e

lExpression :: Parser UExpr
lExpression = liftM UExpression identifier

pExpression :: Parser BExpr
pExpression = liftM2 BExpression identifier identifier

parseString :: String -> Stmt
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Stmt
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

-- FIN PARSE --

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
getName st = n where Stats n e k c o id int p cl cb = st

getExtension :: Stats -> [String]
getExtension st = e where Stats n e k c o id int p cl cb = st

getKeyword :: Stats -> [String]
getKeyword st = k where Stats n e k c o id int p cl cb = st

getConstant :: Stats -> [String]
getConstant st = c where Stats n e k c o id int p cl cb = st

getOperateur :: Stats -> [String]
getOperateur st = o where Stats n e k c o id int p cl cb = st

getIdent :: Stats -> [String]
getIdent st = id where Stats n e k c o id int p cl cb = st

getInt :: Stats -> [String]
getInt st = int where Stats n e k c o id int p cl cb = st

getPaire :: Stats -> [(String,String)]
getPaire st = p where Stats n e k c o id int p cl cb = st

getCommentLine :: Stats -> [String]
getCommentLine st = cl where Stats n e k c o id int p cl cb = st

getCommentBlock :: Stats -> [(String,String)]
getCommentBlock st = cb where Stats n e k c o id int p cl cb = st

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

-- DEBUT EXTRACTION --

uexprtoString :: UExpr -> String
uexprtoString (UExpression s) = tail s

bexprtoString :: BExpr -> (String, String)
bexprtoString (BExpression b e) = (tail b, tail e)

uexprstolist :: [UExpr] -> [String]
uexprstolist [] = []
uexprstolist (x:[]) = [uexprtoString x]
uexprstolist (x:xs) = (uexprtoString x):(uexprstolist xs)

bexprstolist :: [BExpr] -> [(String,String)]
bexprstolist [] = []
bexprstolist (x:[]) = [bexprtoString x]
bexprstolist (x:xs) = (bexprtoString x):(bexprstolist xs)

extractfromStmt :: Stmt -> Stats -> Stats
extractfromStmt (Peers bexprs) st = Stats n e k c o id int (bexprstolist bexprs) cl cb where Stats n e k c o id int _ cl cb = st
extractfromStmt (Extension uexprs) st = Stats n (uexprstolist uexprs) k c o id int p cl cb  where Stats n _ k c o id int p cl cb = st
extractfromStmt (Keywords uexprs) st = Stats n e (uexprstolist uexprs) c o id int p cl cb  where Stats n e _ c o id int p cl cb = st
extractfromStmt (Constants uexprs) st = Stats n e k (uexprstolist uexprs) o id int p cl cb where Stats n e k _ o id int p cl cb = st
extractfromStmt (Operators uexprs) st = Stats n e k c (uexprstolist uexprs) id int p cl cb  where Stats n e k c _ id int p cl cb = st
extractfromStmt (Name s) st = Stats (uexprstolist [(UExpression s)]) e k c o id int p cl cb  where Stats _ e k c o id int p cl cb = st
extractfromStmt (Idents s) st = Stats n e k c o (uexprstolist [(UExpression s)]) int p cl cb  where Stats n e k c o _ int p cl cb = st
extractfromStmt (Numbers s) st = Stats n e k c o id (uexprstolist [(UExpression s)]) p cl cb  where Stats n e k c o id _ p cl cb = st
extractfromStmt (CommentLine s) st = Stats n e k c o id int p (uexprstolist [(UExpression s)]) cb  where Stats n e k c o id int p _ cb = st
extractfromStmt (CommentBlock s1 s2) st = Stats n e k c o id int p cl (bexprstolist [(BExpression s1 s2)]) where Stats n e k c o id int p cl _ = st

extractfromStmts :: [Stmt] -> Stats -> Stats
extractfromStmts [] st = st
extractfromStmts (x:[]) st = extractfromStmt x st
extractfromStmts (x:xs) st = extractfromStmts xs (extractfromStmt x st)

extractData :: Stmt -> IO Stats
extractData (Seq(stmtlist)) = do return (extractfromStmts stmtlist def)
extractData a = do return (extractfromStmt a def)

-- FIN EXTRACTION --

getData file =do
     stmt<-(parseFile file) 
     extractData stmt

genName file= do
    temp<-(getData file)
    return (getName temp) 

genExtension file= do
    temp<-(getData file)
    return (getExtension temp)
genKeyword file=  do
    temp<-(getData file)
    return (getKeyword temp)

genConstant file= do
    temp<-(getData file)
    return  (getConstant temp)

genIdent file= do
    temp<-(getData file)
    return (getIdent temp)
    
genInt file= do
    temp<-(getData file)
    return (getInt temp)

genPaire file= do
    temp<-(getData file)
    return  (getPaire temp)
genCommentLine file= do
    temp<-(getData file)
    return  (getCommentLine temp)
genCommentBlock file= do
    temp<-(getData file)
    return (getCommentBlock temp)
genOperateur file= do
    temp<-(getData file)
    return (getOperateur temp)



