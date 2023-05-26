import System.IO()

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

