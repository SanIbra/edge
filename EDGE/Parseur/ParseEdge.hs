module Parseur.ParseEdge where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Char
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.IORef
import Parseur.DataStructAST
import Parseur.TypeStats

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

whileParser :: Parser Info
whileParser = whiteSpace >> information

information :: Parser Info
information = sequenceOfInfo

sequenceOfInfo =
  do list <- (sepBy1 information' semi)
     -- If there's only one information return it without using Seq.
     return $ if length list == 1 then head list else Seq list

information' :: Parser Info
information' = nameInfo
        <|> extensionInfo
        <|> keywordsInfo
        <|> constantsInfo
        <|> operatorsInfo
        <|> identsInfo
        <|> numbersInfo
        <|> commentLineInfo
        <|> peersInfo
        <|> commentBlockInfo

nameInfo :: Parser Info
nameInfo =
    do reserved "name"
       reservedOp ":"
       n <- identifier
       return $ Name n

extensionInfo :: Parser Info
extensionInfo =
    do reserved "extension"
       reservedOp ":"
       cl <- (sepBy1 lExpression comma)
       return $ Extension cl

keywordsInfo :: Parser Info
keywordsInfo =
    do reserved "keywords"
       reservedOp ":"
       cl <- (sepBy1 lExpression comma)
       return $ Keywords cl
        
constantsInfo :: Parser Info
constantsInfo =
    do reserved "constants"
       reservedOp ":"
       cl <- (sepBy1 lExpression comma)
       return $ Constants cl

operatorsInfo :: Parser Info
operatorsInfo =
    do reserved "operators"
       reservedOp ":"
       cl <- (sepBy1 lExpression comma)
       return $ Operators cl

identsInfo :: Parser Info
identsInfo =
    do reserved "idents"
       reservedOp ":"
       i <- identifier
       return $ Idents i

numbersInfo :: Parser Info
numbersInfo =
    do reserved "numbers"
       reservedOp ":"
       n <- identifier
       return $ Numbers n

commentLineInfo :: Parser Info
commentLineInfo =
    do reserved "commentLine"
       reservedOp ":"
       cl <- identifier
       return $ CommentLine cl

peersInfo :: Parser Info
peersInfo =
    do reserved "peers"
       reservedOp ":"
       cl <- (sepBy1 pExpression comma)
       return $ Peers cl

commentBlockInfo :: Parser Info
commentBlockInfo =
    do reserved "commentBlock"
       reservedOp ":"
       s <- identifier
       e <- identifier
       return $ CommentBlock s e

lExpression :: Parser UExpr
lExpression = liftM UExpression identifier

pExpression :: Parser BExpr
pExpression = liftM2 BExpression identifier identifier

parseString :: String -> Info
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Info
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

-- FIN PARSE --



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

extractfromInfo :: Info -> Stats -> Stats
extractfromInfo (Peers bexprs) st = Stats n e k c o id int (bexprstolist bexprs) cl cb where Stats n e k c o id int _ cl cb = st
extractfromInfo (Extension uexprs) st = Stats n (uexprstolist uexprs) k c o id int p cl cb  where Stats n _ k c o id int p cl cb = st
extractfromInfo (Keywords uexprs) st = Stats n e (uexprstolist uexprs) c o id int p cl cb  where Stats n e _ c o id int p cl cb = st
extractfromInfo (Constants uexprs) st = Stats n e k (uexprstolist uexprs) o id int p cl cb where Stats n e k _ o id int p cl cb = st
extractfromInfo (Operators uexprs) st = Stats n e k c (uexprstolist uexprs) id int p cl cb  where Stats n e k c _ id int p cl cb = st
extractfromInfo (Name s) st = Stats (uexprstolist [(UExpression s)]) e k c o id int p cl cb  where Stats _ e k c o id int p cl cb = st
extractfromInfo (Idents s) st = Stats n e k c o (uexprstolist [(UExpression s)]) int p cl cb  where Stats n e k c o _ int p cl cb = st
extractfromInfo (Numbers s) st = Stats n e k c o id (uexprstolist [(UExpression s)]) p cl cb  where Stats n e k c o id _ p cl cb = st
extractfromInfo (CommentLine s) st = Stats n e k c o id int p (uexprstolist [(UExpression s)]) cb  where Stats n e k c o id int p _ cb = st
extractfromInfo (CommentBlock s1 s2) st = Stats n e k c o id int p cl (bexprstolist [(BExpression s1 s2)]) where Stats n e k c o id int p cl _ = st

extractfromInfos :: [Info] -> Stats -> Stats
extractfromInfos [] st = st
extractfromInfos (x:[]) st = extractfromInfo x st
extractfromInfos (x:xs) st = extractfromInfos xs (extractfromInfo x st)

extractData :: Info -> IO Stats
extractData (Seq(infolist)) = do return (extractfromInfos infolist def)
extractData a = do return (extractfromInfo a def)

-- FIN EXTRACTION --
getData::String-> IO Stats
getData file =do
     infos <-(parseFile file) 
     extractData infos

