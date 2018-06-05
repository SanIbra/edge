module Generateur.Option where

data Option = Opt [String]

listOption::[String]
listOption = ["only","base"]


getOption::[String]->Int->[Option]
getOption option n=
    if (n==(length option))
        then []
        else (if (isOption (option!!n))
              then (let end=(getEnd option (n+1)) in (Opt (makeOption option n end)):(getOption option (end+1)))
              else (Opt ["ERREUR",""]):[])


----------DEBUG FONCTION--------------------------------
printtest ((Opt x):[]) = putStrLn ((concatTest x)++"\nFIN")
printtest ((Opt x):y) =do putStrLn (concatTest x)
                          printtest y
concatTest []=""
concatTest (x:xs)=x++" "++(concatTest xs)
---------------------------------------------------------
isOption::String->Bool
isOption candidat =(any (== (tail candidat)) listOption)


getHead::Option->String
getHead (Opt o)=head o

getTail::Option->[String]
getTail (Opt o)=tail o

getOptionArgs::[Option]->String->[String]
getOptionArgs [] nameOpt =[]
getOptionArgs (op1:resOpt) nameOpt =if ( ("-"++nameOpt)==(getHead op1) ) then ((getTail op1)++(getOptionArgs resOpt nameOpt)) else (getOptionArgs resOpt nameOpt)
     
isPresent::[Option]->String->Bool
isPresent [] nom=False
isPresent (op1:resOpt) nom=if ( ("-"++(getHead op1))==nom) then True else (isPresent resOpt nom)

getEnd::[String]->Int->Int
getEnd option n=if ( (((length option))==n) || (head(option!!n)=='-')) then n-1 else getEnd option (n+1)
makeOption::[String]->Int->Int->[String]
makeOption option debut fin= if (debut-1/=fin) then (option!!debut):(makeOption option (debut+1) fin) else [] 