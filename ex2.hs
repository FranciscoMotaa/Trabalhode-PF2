import System.IO(openFile, hPutStrLn, hClose, hPutStr)
import Prelude
import Data.List


solucaodistribuicao :: IO()
solucaodistribuicao  = do 
    conteudoDisciplina <- readFile "ucs.txt"

    -- Limpa o conteúdo do arquivo "solucao.txt"
    ficheiro <- openFile "solucao.txt" WriteMode
    hClose ficheiro

    -- Fazer uma cópia do conteúdo de "ucs.txt" para "Suporte2.txt"
    conteudoUCS <- readFile "ucs.txt"
    ficheiro2 <- openFile "Suporte2.txt" WriteMode
    hPutStr ficheiro2 conteudoUCS
    hClose ficheiro2

    -- Criar o arquivo "Suporte3.txt" vazio
    ficheiro3 <- openFile "Suporte3.txt" WriteMode
    hClose ficheiro3

    -- Criar o arquivo "Suporte.txt" e escrever "0" nele
    ficheiro <- openFile "Suporte.txt" WriteMode
    hPutStrLn ficheiro "0"
    hClose ficheiro

    
    --descobrir qual o ano mais alto escrever no ficheiro 1

    copyFile (lines conteudoDisciplina)

    suporte2 <- readFile' "Suporte2.txt"
    maiorAno (lines suporte2)
    let suporte = readFile' "Suporte.txt"
    maiorAnoString <- suporte
    let maiorAnoInt = read maiorAnoString :: Int -- maior ano no ficheiro
    

    --Pedir input
    putStrLn "indicate number of days in which the exam can take place"
    dias <- getLine
    putStrLn "indicate the number of rooms available per day"
    salas <- getLine
    --linhas no ficheiro-
    let n_disciplinas = length (lines conteudoDisciplina) -- numero de disciplinas é o numero de linhas no ficheiro
    --convercao-- para int
    let n_dias = read dias :: Int
    let n_salas = read salas :: Int

    distribuir2 maiorAnoInt 1 n_dias n_salas (lines conteudoDisciplina)

    suporte2 <- readFile' "Suporte2.txt"

    if null (lines suporte2)
        then return()
        else do 
            putStrLn "Insufficient days to accommodate all exams"
            return()



--funcoes tarefa 2 
maiorAno :: [String] -> IO()
maiorAno [] = return()
maiorAno (x:xs) = do 

    let suporte = readFile' "Suporte.txt"
    
    suporteString <-  suporte
    let suporteInt = read suporteString :: Int

    let numero = head(tail(words x))
    let numeroInt = read numero :: Int

    if numeroInt > suporteInt
        then do 
            ficheiro <- openFile "Suporte.txt" WriteMode
            hPrint ficheiro numeroInt
            hClose ficheiro
            maiorAno xs
        else maiorAno xs
            
distribuir2 :: Int -> Int -> Int -> Int -> [String] -> IO() 
distribuir2 anoMax dias diasMax salas [] = return()
distribuir2 anoMax dias diasMax salas (x:xs) = do

    if dias > diasMax
        then return()
        else do 

            ficheiro <- openFile "solucao.txt" AppendMode
            hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
            hClose ficheiro

            ficheiro <- openFile "Suporte3.txt" WriteMode
            hClose ficheiro --limpar ficheiro3

            --funcao que seleciona disciplinas para ficheiro3 e apaga o ficheiro 2
            repeater2 anoMax salas
            
            --ficheiro3 a zero

            suporte3 <- readFile' "Suporte3.txt"
            printSalas2 salas (lines suporte3)--print nos ficheiros do 3

            suporte2 <- readFile' "Suporte2.txt"
            if null (lines suporte2)
                then return()
                else do
                    ficheiro <- openFile "Suporte.txt" WriteMode
                    hPutStrLn ficheiro "0"
                    hClose ficheiro
                    maiorAno (lines suporte2)
                    let suporte = readFile' "Suporte.txt"
                    maiorAnoString <- suporte
                    let maiorAnoInt = read maiorAnoString :: Int -- maior ano no ficheiro
                    distribuir2 maiorAnoInt (dias+1) diasMax salas (lines suporte2)

finder :: Int -> [String] -> IO()
finder _ [] = return()
finder ano (x:xs) = do --ano a encontrar, ficheiro2
    let anoLinhaString = head(tail(words x))
    let anoLinhaInt = read anoLinhaString :: Int

    if ano == anoLinhaInt
        then do
            ficheiro <- openFile "Suporte3.txt" AppendMode
            hPutStrLn ficheiro x
            hClose ficheiro --limpar ficheiro3
            copyFile xs
            return()
        else do
            ficheiro <- openFile "Suporte2.txt" AppendMode
            hPutStrLn ficheiro x
            hClose ficheiro 
            finder ano xs

 
repeater2 :: Int -> Int -> IO()
repeater2 0 _ = return()
repeater2 _ 0 = return()
repeater2 anoMax salas = do --ano maximo a procurar e numero de salas a preencher
    suporte2 <- readFile' "Suporte2.txt"

    ficheiro <- openFile "Suporte2.txt" WriteMode
    hClose ficheiro --limpar ficheiro2

    finder anoMax (lines suporte2)
    repeater2 (anoMax-1)(salas-1)


printSalas2 :: Int -> [String] -> IO()
printSalas2 salas [] = return()
printSalas2 0 (x:xs) = return()
printSalas2 salas (x:xs) = do
    ficheiro <- openFile "solucao.txt" AppendMode
    hPutStrLn ficheiro ("Sala "++ show salas ++ ": "++ unwords (tail (tail(words x))))
    hClose ficheiro
    printSalas2 (salas-1) xs
