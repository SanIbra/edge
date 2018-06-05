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
import Parseur.Regex

-- Utilitary
forIdent c =
    if c == ' ' || c == '\t' || c == '\n' || c == '\r' then False else True

-- language definition
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

-- Create the lexer
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

-- BEGIN PARSING --

whileParser :: Parser Info
whileParser = whiteSpace >> information

information :: Parser Info
information = sequenceOfInfo

-- Parses all information separated by 1 semicolon
sequenceOfInfo =
  do list <- (sepBy1 information' semi)
     -- If there's only one information return it without using Seq.
     return $ if length list == 1 then head list else Seq list

-- Parses information
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

-- Parses name
nameInfo :: Parser Info
nameInfo =
    do reserved "name"
       reservedOp ":"
       n <- identifier
       return $ Name n

-- Parses extension
extensionInfo :: Parser Info
extensionInfo =
    do reserved "extension"
       reservedOp ":"
       cl <- (sepBy1 lExpression comma)
       return $ Extension cl

-- Parses keywords
keywordsInfo :: Parser Info
keywordsInfo =
    do reserved "keywords"
       reservedOp ":"
       cl <- (sepBy1 lExpression comma)
       return $ Keywords cl
        
-- Parses constants
constantsInfo :: Parser Info
constantsInfo =
    do reserved "constants"
       reservedOp ":"
       cl <- (sepBy1 lExpression comma)
       return $ Constants cl

-- Parses operators
operatorsInfo :: Parser Info
operatorsInfo =
    do reserved "operators"
       reservedOp ":"
       cl <- (sepBy1 lExpression comma)
       return $ Operators cl

-- Parses identificator regex
identsInfo :: Parser Info
identsInfo =
    do reserved "idents"
       reservedOp ":"
       i <- identifier
       return $ Idents i

-- Parses numbers regex
numbersInfo :: Parser Info
numbersInfo =
    do reserved "numbers"
       reservedOp ":"
       n <- identifier
       return $ Numbers n

-- Parses comment line start symbol
commentLineInfo :: Parser Info
commentLineInfo =
    do reserved "commentLine"
       reservedOp ":"
       cl <- identifier
       return $ CommentLine cl

-- Parses peers symbols
peersInfo :: Parser Info
peersInfo =
    do reserved "peers"
       reservedOp ":"
       cl <- (sepBy1 pExpression comma)
       return $ Peers cl

-- Parses commentBlock start and end symbols
commentBlockInfo :: Parser Info
commentBlockInfo =
    do reserved "commentBlock"
       reservedOp ":"
       s <- identifier
       e <- identifier
       return $ CommentBlock s e

-- Parses a UExpr
lExpression :: Parser UExpr
lExpression = liftM UExpression identifier

-- Parses a BExpr
pExpression :: Parser BExpr
pExpression = liftM2 BExpression identifier identifier

-- Test function that parses a string
parseString :: String -> Info
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

-- Parses a file with the name given in parameter
parseFile :: String -> IO Info
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

-- END PARSING --



-- BEGIN EXTRACTION --
-- If you add something here and don't use one of the 3 functions underneath this comment
-- don't forget to use "tail" to remove the # at the begining of the string you extract

-- extract a string from a UExpr
uexprtoString :: UExpr -> String
uexprtoString (UExpression s) = tail s

-- extract a regex from a UExpr
uexprtorRegex :: UExpr -> Regex
uexprtorRegex (UExpression s) = RE (tail s)

-- extract a tuple from a BExpr
bexprtoString :: BExpr -> (String, String)
bexprtoString (BExpression b e) = (tail b, tail e)

-- use uexprtoString each element of the list and return the list that result (could've used map)
uexprstolist :: [UExpr] -> [String]
uexprstolist [] = []
uexprstolist (x:[]) = [uexprtoString x]
uexprstolist (x:xs) = (uexprtoString x):(uexprstolist xs)

-- use uexprtorRegex each element of the list and return the list that result (could've used map)
uexprstoregexlist :: [UExpr] -> [Regex]
uexprstoregexlist [] = []
uexprstoregexlist (x:[]) = [uexprtorRegex x]
uexprstoregexlist (x:xs) = (uexprtorRegex x):(uexprstoregexlist xs)

-- use bexprtoString each element of the list and return the list that result (could've used map)
bexprstolist :: [BExpr] -> [(String,String)]
bexprstolist [] = []
bexprstolist (x:[]) = [bexprtoString x]
bexprstolist (x:xs) = (bexprtoString x):(bexprstolist xs)

-- extract information from an Info node and put it in the Stats
extractfromInfo :: Info -> Stats -> Stats
extractfromInfo (Peers bexprs) st = Stats n e k c o id int (bexprstolist bexprs) cl cb where Stats n e k c o id int _ cl cb = st
extractfromInfo (Extension uexprs) st = Stats n (uexprstolist uexprs) k c o id int p cl cb  where Stats n _ k c o id int p cl cb = st
extractfromInfo (Keywords uexprs) st = Stats n e (uexprstolist uexprs) c o id int p cl cb  where Stats n e _ c o id int p cl cb = st
extractfromInfo (Constants uexprs) st = Stats n e k (uexprstolist uexprs) o id int p cl cb where Stats n e k _ o id int p cl cb = st
extractfromInfo (Operators uexprs) st = Stats n e k c (uexprstolist uexprs) id int p cl cb  where Stats n e k c _ id int p cl cb = st
extractfromInfo (Name s) st = Stats (uexprstolist [(UExpression s)]) e k c o id int p cl cb  where Stats _ e k c o id int p cl cb = st
extractfromInfo (Idents s) st = Stats n e k c o (uexprstoregexlist [(UExpression s)]) int p cl cb  where Stats n e k c o _ int p cl cb = st
extractfromInfo (Numbers s) st = Stats n e k c o id (uexprstoregexlist [(UExpression s)]) p cl cb  where Stats n e k c o id _ p cl cb = st
extractfromInfo (CommentLine s) st = Stats n e k c o id int p (uexprstolist [(UExpression s)]) cb  where Stats n e k c o id int p _ cb = st
extractfromInfo (CommentBlock s1 s2) st = Stats n e k c o id int p cl (bexprstolist [(BExpression s1 s2)]) where Stats n e k c o id int p cl _ = st

-- use extractfromInfo on a list and return the last Stats obtained (could've used foldl)
extractfromInfos :: [Info] -> Stats -> Stats
extractfromInfos [] st = st
extractfromInfos (x:[]) st = extractfromInfo x st
extractfromInfos (x:xs) st = extractfromInfos xs (extractfromInfo x st)

-- main Extract function
extractData :: Info -> Stats -> IO Stats
extractData (Seq(infolist)) def = do return (extractfromInfos infolist def)
extractData a def = do return (extractfromInfo a def)

-- END EXTRACTION --

-- Main function
getData::String-> Stats -> IO Stats
getData file stat =do
     infos <-(parseFile file)
     extractData infos stat