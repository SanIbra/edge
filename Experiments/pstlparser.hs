module ParseWhile where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Char
import qualified Text.ParserCombinators.Parsec.Token as Token

-- description du langage
-- a ::= extension | keywords | constants | operators

-- b ::= b , b | x

-- c ::= peers

-- d ::= d , d | x x

-- u ::= name : x | idents : x | numbers : x | commentLine : x | commentBlock : x x

-- S ::= S1; S2 | a : b | c : d | u

-- création des structures de données
data UExpr = UExpression String | USequence UExpr UExpr deriving (Show) 

data BExpr = BExpression UExpr UExpr | BSequence BExpr BExpr deriving (Show) 

data Stmt = Seq [Stmt]
          | Peers BExpr
          | Extension UExpr
          | Keywords UExpr
          | Constants UExpr
          | Operators UExpr
          | Name String 
          | Idents String 
          | Numbers String 
          | CommentLine String 
          | CommentBlock String String
          | PeerType
          | LType
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
       cl <- lExpression
       return $ Extension cl

keywordsStmt :: Parser Stmt
keywordsStmt =
    do reserved "keywords"
       reservedOp ":"
       cl <- lExpression
       return $ Keywords cl
        
constantsStmt :: Parser Stmt
constantsStmt =
    do reserved "constants"
       reservedOp ":"
       cl <- lExpression
       return $ Constants cl

operatorsStmt :: Parser Stmt
operatorsStmt =
    do reserved "operators"
       reservedOp ":"
       cl <- lExpression
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
       cl <- pExpression
       return $ Peers cl

commentBlockStmt :: Parser Stmt
commentBlockStmt =
    do reserved "commentBlock"
       reservedOp ":"
       s <- identifier
       e <- identifier
       return $ CommentBlock s e

lExpression :: Parser UExpr
lExpression = buildExpressionParser bUOperator sTerm

pExpression :: Parser BExpr
pExpression = buildExpressionParser bBOperator pTerm

bUOperator = [ [Infix (reservedOp "," >> return (USequence )) AssocLeft]]

bBOperator = [ [Infix (reservedOp "," >> return (BSequence )) AssocLeft]]

sTerm = liftM UExpression identifier

-- pTerm = BExpression (liftM UExpression identifier) (liftM UExpression identifier)
pTerm = do a <- liftM UExpression identifier
           b <- liftM UExpression identifier
           return $ BExpression a b

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