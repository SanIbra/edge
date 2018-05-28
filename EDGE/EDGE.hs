module GenerateurTest where

import System.IO
import System.Directory
import VsCode
import Vim
import Emacs

mode=["vscode","vim","emacs"]
listMode = putStrLn (aff mode)
aff (m:[])=m
aff (m:reste)=m++"\n"++(aff reste)

edgeGen file "vscode"= vsCodeExtension file
edgeGen file "vim"= vimgene  file
edgeGen file "emacs"= emacsMode  file
edgeGen file other= putStrLn ("No existing mode for "++other)



