import System.IO()

leralunos :: IO()
leralunos = do
    alunos <- readFile "listaalunos.txt"
    calunos2 (lines alunos)

calunos2 :: [String] -> IO()
calunos2 [] = return ()
calunos2 (x:xs) = do
    putStrLn (unwords(tail(tail(words x))))
    inscricoes <- readFile "inscricoes.txt"
    if not (null inscricoes) then 
        cinsc2 (head(words x)) (lines inscricoes)
    else 
        putStrLn "Lista de inscrições vazia"
    calunos2 xs 

cinsc2 :: String -> [String] -> IO()
cinsc2 ident [] = return ()   
cinsc2 ident (x:xs) = if ident == head(words x) then 
    do
        ucs <- readFile "ucs.txt"
        if not (null ucs) then
            cucs2 (last(words x)) (lines ucs)
        else 
            putStrLn "Lista de Ucs vazia"
        cinsc2 ident xs 
        else 
            cinsc2 ident xs 

cucs2 :: String -> [String] -> IO ()
cucs2 uc [] = return ()
cucs2 uc (x:xs) = if uc == head(words x) then 
    do 
        putStrLn (unwords(tail(tail(words x))))
        else 
            cucs2 uc xs
            

