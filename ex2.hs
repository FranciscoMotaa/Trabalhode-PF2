--------
import System.IO
import Data.List (nub)
import Prelude

--tarefa2
tarefa2 :: IO()
tarefa2 = do 
    conteudoDisciplina <- readFile "ucs.txt"
    -- Criar e fechar arquivo "Tarefa2.txt" para limpar seu conteúdo
    ficheiro <- openFile "Tarefa2.txt" WriteMode
    hClose ficheiro
    ---- Criar e fechar arquivo "Suporte2.txt" para limpar seu conteúdo
    ficheiro <- openFile "Suporte2.txt" WriteMode
    hClose ficheiro
 -- Criar e fechar arquivo "Suporte3.txt" para limpar seu conteúdo
    ficheiro <- openFile "Suporte3.txt" WriteMode
    hClose ficheiro
 -- Criar arquivo "Suporte.txt" com valor inicial 0
    ficheiro <- openFile "Suporte.txt" WriteMode
    hPutStrLn ficheiro "0"
    hClose ficheiro
    -- Descobrir qual é o ano mais alto e escrever no arquivo "Suporte.txt"

    copyFile (lines conteudoDisciplina)

    suporte2 <- readFile' "Suporte2.txt"
    maiorAno (lines suporte2)
    let suporte = readFile' "Suporte.txt"
    maiorAnoString <- suporte
    let maiorAnoInt = read maiorAnoString :: Int -- maior ano no ficheiro
    

    -- Pedir input do usuário
    putStrLn "indique numero de dias em que o exame pode ocorrer"
    dias <- getLine
    putStrLn "indique o numero de salas disponiveis por dia"
    salas <- getLine
    -- Obter o número de disciplinas no arquivo
    let n_disciplinas = length (lines conteudoDisciplina) -- numero de disciplinas é o numero de linhas no ficheiro
    --convercao-- para int
    let n_dias = read dias :: Int
    let n_salas = read salas :: Int
   -- Chamar a função de escalonamento
    escalonamento2 maiorAnoInt 1 n_dias n_salas (lines conteudoDisciplina)
-- Ler o arquivo "Suporte2.txt" para verificar se todas as disciplinas foram alocadas
    suporte2 <- readFile' "Suporte2.txt"

    if null (lines suporte2)
        then return()
        else do 
            putStrLn "Tempo insuficiente para acomodar todos os exames"
            return()
--funcoes tarefa 2
-- Função para encontrar o maior ano curricular no arquivo "Suporte2.txt" 
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
  -- Função de escalonamento          
escalonamento2 :: Int -> Int -> Int -> Int -> [String] -> IO() 
escalonamento2 anoMax dias diasMax salas [] = return()
escalonamento2 anoMax dias diasMax salas (x:xs) = do

    if dias > diasMax
        then return()
        else do 
-- Abrir arquivo "Tarefa2.txt" em modo AppendMode para adicionar conteúdo
            ficheiro <- openFile "Tarefa2.txt" AppendMode
            hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
            hClose ficheiro
-- Criar e fechar arquivo "Suporte3.txt" para limpar seu conteúdo
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
                    -- Limpar arquivo "Suporte3.txt"
                    ficheiro <- openFile "Suporte.txt" WriteMode
                    hPutStrLn ficheiro "0"
                    hClose ficheiro
                    maiorAno (lines suporte2)
                    let suporte = readFile' "Suporte.txt"
                    maiorAnoString <- suporte
                    let maiorAnoInt = read maiorAnoString :: Int -- maior ano no ficheiro
                    escalonamento2 maiorAnoInt (dias+1) diasMax salas (lines suporte2)
-- Função para encontrar as disciplinas com o ano curricular igual ao anoMax no arquivo "Suporte2.txt"
finder :: Int -> [String] -> IO()
finder _ [] = return()
finder ano (x:xs) = do --ano a encontrar, ficheiro2
    let anoLinhaString = head(tail(words x))
    let anoLinhaInt = read anoLinhaString :: Int

    if ano == anoLinhaInt
        then do
             -- Adicionar a linha no arquivo "Suporte3.txt"
            ficheiro <- openFile "Suporte3.txt" AppendMode
            hPutStrLn ficheiro x
            hClose ficheiro --limpar ficheiro3
            copyFile xs
            return()
        else do
             -- Adicionar a linha no arquivo "Suporte2.txt
            ficheiro <- openFile "Suporte2.txt" AppendMode
            hPutStrLn ficheiro x
            hClose ficheiro 
            finder ano xs

-- Função para repetir a busca por disciplinas do ano curricular máximo no arquivo "Suporte2.txt" e adicionar no arquivo "Suporte3.txt"
repeater2 :: Int -> Int -> IO()
repeater2 0 _ = return()
repeater2 _ 0 = return()
repeater2 anoMax salas = do --ano maximo a procurar e numero de salas a preencher
    suporte2 <- readFile' "Suporte2.txt"
    -- Limpar arquivo "Suporte2.txt"
    ficheiro <- openFile "Suporte2.txt" WriteMode
    hClose ficheiro --limpar ficheiro2

    finder anoMax (lines suporte2)
    repeater2 (anoMax-1)(salas-1)

-- Função para imprimir as salas nos arquivos do "Suporte3.txt"
printSalas2 :: Int -> [String] -> IO()
printSalas2 salas [] = return()
printSalas2 0 (x:xs) = return()
printSalas2 salas (x:xs) = do
  -- Abrir arquivo "Tarefa2.txt" em modo AppendMode para adicionar conteúdo
    ficheiro <- openFile "Tarefa2.txt" AppendMode
    hPutStrLn ficheiro ("Sala "++ show salas ++ ": "++ unwords (tail (tail(words x))))
    hClose ficheiro
    printSalas2 (salas-1) xs
-- Função para copiar o conteúdo de uma lista de strings para o arquivo "Suporte2.txt"
copyFile :: [String] -> IO()
copyFile [] = return()
copyFile (x:xs)= do 
    ficheiro <- openFile "Suporte2.txt" AppendMode
    hPutStrLn ficheiro x
    hClose ficheiro
    copyFile xs
