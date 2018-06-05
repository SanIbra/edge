
import System.Environment
import Mode.VsCode
import Mode.Vim
import Mode.Emacs
import Generateur.FileGenerator
import Parseur.ParseEdge
import Parseur.TypeStats
import Generateur.Option

mode=["vscode","vim","emacs"]

listMode::IO()
listMode = putStrLn (aff mode)
aff (m:[])=m
aff (m:reste)=m++"\n"++(aff reste)

main=do 
    option<-getArgs 
    if (length option)==0
        then putStrLn "No argument enter"
        else
            case (option!!0) of 
                "-listmode" -> listMode
                otherwise   -> (gen otherwise (getOption option 1))
                
gen::String->[Option]->IO ()
gen fichier option=do
    --Option sur l'AST
    stat<-(if (isPresent option "base") || ((length (getOptionArgs option "base"))==0) 
             then (getData fichier defC)
             else (getData fichier (getDefault (getOptionArgs option "base"))))
    --Option de genenerateir
    generateList (if (length (getOptionArgs option "only"))/=0
                    then (getToGenerateAll stat (getOptionArgs option "only"))
                    else (getToGenerateAll stat mode))

getDefault::[String]->Stats
getDefault []=defC
getDefault (x:_)=case x of 
                    "python"->defPy
                    "ocaml"->defCam
                    otherwise->defC

edgeGen::String->String->IO()
edgeGen fichier mode=do
    stat <-(getData fichier defC)
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

