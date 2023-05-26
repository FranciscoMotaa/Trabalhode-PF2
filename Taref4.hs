import System.IO()

alunoop :: IO()
alunoop = do
    inscricoes <- readFile "inscricoes.txt"
    putStrLn "Introduza o número correspondente ao aluno para ver a que Unidades curriculares está matriculado"
    putStrLn "1 -> Rui Silva"
    putStrLn "2 -> Maria Silva"
    putStrLn "3 -> Tiago Silva"
    putStrLn "4 -> Sofia Silva"
    op <- getLine
    case op of                          
        "1" -> do 
            let matr = "al001"
            cinsc4 matr (lines inscricoes)
        "2" -> do
            let matr = "al002"
            cinsc4 matr (lines inscricoes)
        "3" -> do
            let matr = "al003"
            cinsc4 matr (lines inscricoes)
        "4" -> do
            let matr = "al004"
            cinsc4 matr (lines inscricoes)
        _   -> putStrLn "Erro"

cinsc4 :: String -> [String] -> IO()
cinsc4 ident [] = return ()   
cinsc4 ident (x:xs) = if ident == head(words x) then 
    do
        ucs <- readFile "ucs.txt"
        if not (null ucs) then
            cucs4 (last(words x)) (lines ucs)
        else 
            putStrLn "Lista de Ucs vazia"
        cinsc4 ident xs 
        else 
            cinsc4 ident xs 

cucs4 :: String -> [String] -> IO ()
cucs4 uc [] = return ()
cucs4 uc (x:xs) = if uc == head(words x) then 
    do 
        putStrLn (unwords(tail(tail(words x))))
        else 
            cucs4 uc xs