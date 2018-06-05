module Parseur.DataStructAST where

-- data struct for the AST 
data UExpr = UExpression String deriving (Show) 

data BExpr = BExpression String String deriving (Show) 

data Info = Seq [Info]
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