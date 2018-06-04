module EdgeGen where


import Mode.VsCode
import Mode.Vim
import Mode.Emacs
import FileGenerator
import Parseur.ParseEdge
import Parseur.TypeStats

mode=["vscode","vim","emacs"]

listMode::IO()
listMode = putStrLn (aff mode)
aff (m:[])=m
aff (m:reste)=m++"\n"++(aff reste)

edgeGen::String->String->IO()
edgeGen fichier mode=do 
    stat <-(getData fichier)
    generateList (getToGenerate stat mode)

generateList::[ToGenerate]->IO()
generateList (x:[])=generateFile x
generateList (x:xs)=do 
    generateFile x
    generateList xs

getToGenerateAll::Stats->[String]->[ToGenerate]
getToGenerateAll descripteur []=[]
getToGenerateAll descripteur (x:xs) =(getToGenerate descripteur x)++(getToGenerateAll descripteur xs)

getToGenerate:: Stats->String->[ToGenerate]
getToGenerate descripteur "all"= (getToGenerateAll descripteur mode)
getToGenerate descripteur "vscode"= vsCodeExtension descripteur
getToGenerate descripteur "vim"= vimgene  descripteur
getToGenerate descripteur "emacs"= (emacsMode  descripteur)
getToGenerate descripteur other=[]-- putStrLn ("No existing mode for "++other)

