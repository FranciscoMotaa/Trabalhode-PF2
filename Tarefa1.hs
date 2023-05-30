{-Criar um novo ficheiro que contenha um escalonamento dos exames (apenas para uma época 
de exames) para todas as UCs, considerando o dia e sala em que cada exame ocorrerá. Deverá 
ser apresentado um aviso no terminal caso o número de salas e dias disponíveis sejam 
insuficientes para acomodar todos os exames necessários, sendo que não poderá ocorrer 
mais que um exame na mesma sala no mesmo dia (5 valores).-}

--import System.IO(hPutStrLn)
import System.IO ( hClose, hPutStrLn, openFile, IOMode(WriteMode) )

tarefa1 :: IO ()
tarefa1 = do
    putStrLn "Introduza o número total de dias em que os exames podem ocorrer: "
    dias <- getLine
    putStrLn "Introduza o número total de salas disponíveis: "
    salas <- getLine
    escreveFicheiro "exemplo.txt" (dias ++ " " ++ salas)

escreveFicheiro :: FilePath -> String -> IO ()
escreveFicheiro nomeArquivo conteudo = do
    arquivo <- openFile nomeArquivo WriteMode
    hPutStrLn arquivo conteudo
    hClose arquivo





