module Generateur.FileGenerator where

import System.IO
import System.Directory

data ToGenerate = File (String,String)
                | Folders [String] 
                deriving (Show)

-- Create a File/Folder from a ToGenerate
generateFile::ToGenerate -> IO ()
generateFile (File (fileName,contents)) = createFile fileName contents
generateFile (Folders folder)=createFolder folder

createFile:: String -> String -> IO ()
createFile fileName contents= do 
    file<-(openFile fileName) WriteMode
    hPutStrLn file contents
    hClose file


createFolder::[String]-> IO ()
createFolder (folder:[])=do
    createDirectory folder

createFolder (folder:reste) = do
    createDirectory folder
    createFolder reste



