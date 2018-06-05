module Parseur.Regex where

data Regex = RE String deriving (Show)

-- uses the function f on each char of the string
modifie :: (Char -> String) -> String -> String
modifie f [] = []
modifie f (x:xs) = (f x)++(modifie f xs)

-- as modifie but if a '\\' character is encountered, remove the '\\' if the character after it is in the list e
modifieExcept :: (Char -> String) -> String -> String -> String
modifieExcept f e [] = []
modifieExcept f e ('\\':x:xs) = if (any (== x) e) then x:(modifieExcept f e xs) else '\\':x:(modifieExcept f e xs)
modifieExcept f e (x:xs) = (f x)++(modifieExcept f e xs)

-- VSCode functions
charRegtoVS :: Char -> String
charRegtoVS '\\' = "\\\\"
charRegtoVS '"' = "\\\""
charRegtoVS c = [c]

toVSCode :: Regex -> String
toVSCode (RE s) = modifie charRegtoVS s

--Vim functions
charRegtoVim :: Char -> String
charRegtoVim c = if (any (== c) "+()|\"") then '\\':[c] else [c]

toVim :: Regex -> String
toVim (RE s) = modifieExcept charRegtoVim "+()|" s

-- Emacs functions
charRegtoEmacs :: Char -> String
charRegtoEmacs '|' = "\\\\|"
charRegtoEmacs '"' = "\\\\\""
charRegtoEmacs '\\' = "\\\\"
charRegtoEmacs c = [c]

toEmacs :: Regex -> String
toEmacs (RE s) = modifieExcept charRegtoEmacs "|" s

-- functions to transform symbols or strings to put them in regex format (Not all the cases are handled)
charStringtoReg :: Char -> String
charStringtoReg c = if (any (== c) "\\*.+^?()|[]{}$") then '\\':[c] else [c]

fromStringtoReg :: String -> Regex
fromStringtoReg s = RE (modifie charStringtoReg s)

fromPeertoRegs :: (String,String) -> (Regex,Regex)
fromPeertoRegs (s1,s2) = (fromStringtoReg s1, fromStringtoReg s2)

fromPeerstoRegs :: [(String,String)] -> [(Regex,Regex)]
fromPeerstoRegs [] = []
fromPeerstoRegs (x:xs) = (fromPeertoRegs x):(fromPeerstoRegs xs)

fromSymboltoString :: String -> String
fromSymboltoString "" = ""
fromSymboltoString ('\"':xs) = '\\':'\"':(fromSymboltoString xs)
fromSymboltoString (c:xs) = c:(fromSymboltoString xs)