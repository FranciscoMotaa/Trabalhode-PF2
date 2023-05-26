import System.IO()

lerucs :: IO()
lerucs = do
    ucs <- readFile "ucs.txt"
    correrucs (lines ucs) 

correrucs :: [String] -> IO()
correrucs [] = return ()
correrucs (x:xs)  = do
        putStrLn (unwords(tail(tail(words x))))
        inscricoes <- readFile "inscricoes.txt"
        correrinsc1 (head(words x)) (lines inscricoes)  
        correrucs xs  

correrinsc1 :: String -> [String] -> IO()
correrinsc1 ucs [] = return ()
correrinsc1 ucs (x:xs) = 
    if ucs == last(words x) then
    do
        alunos <- readFile "listaalunos.txt"
        correralunos1 (head(words x)) (lines alunos)
        correrinsc1 ucs xs 
        else
            correrinsc1 ucs xs

correralunos1 :: String -> [String] -> IO ()
correralunos1 al [] = return ()
correralunos1 al (x:xs) = 
    if al == head(words x) then
    do
        putStrLn(unwords(tail(tail(words x))))
    else
        correralunos1 al xs

--Tarefa 2 

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

--Tarefa 3

opcaoUc :: IO()
opcaoUc = do 
    putStrLn "Introduza a unidade curricular que quer visualizar\n [1]-Programação Funcional\n[2]-Compiladores\n[3]-Tópicos\n[4]-Física"
    op <- getLine
    inscricoes <- readFile "inscricoes.txt"
    correrinsc3 op (lines inscricoes)

correrinsc3 :: String -> [String] -> IO()
correrinsc3 ucs [] = return ()
correrinsc3 op (x:xs) = 
    if op == last(words x) 
        then
            do
                alunos <- readFile "listaalunos.txt"
                correralunos3 (head(words x)) (lines alunos)
                correrinsc3 op xs 
        else
            correrinsc3 op xs

correralunos3 :: String -> [String] -> IO ()
correralunos3 al [] = return ()
correralunos3 al (x:xs) = 
    if al == head(words x) 
        then
            do
                putStrLn(unwords(tail(tail(words x))))
    else
        correralunos3 al xs

--tarefa 4

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

main :: IO ()
main = do 
    putStrLn "Bem vindo ao Programa de ver os alunos inscritos nas unidades curriculares!"
    putStrLn "-------------MENU DE OPÇÕES-------------"
    putStrLn "[1]-Apresentar todos os alunos que estão inscritos em cada unidade curricular"
    putStrLn "[2]-Apresentar todas as unidades curriculares ás quais cada aluno está inscrito"
    putStrLn "[3]-Selecionar a unidade curricular para ver quais alunos estão inscritos nela"
    putStrLn "[4]-Selecionar o aluno para ver a que unidades curriculares está inscrito"
    op <- getLine 
    opcaomenu op 

opcaomenu :: String -> IO()
opcaomenu op 
    | op == "1" =
        do
            lerucs
            main 
    | op == "2" =
        do 
            leralunos
            main
    | op == "3" =
        do
            opcaoUc
            main
    | op == "4" = 
        do
            alunoop
            main
    | otherwise = putStrLn "Opção inválida!"