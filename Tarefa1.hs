{-Criar um novo ficheiro que contenha um escalonamento dos exames (apenas para uma época 
de exames) para todas as UCs, considerando o dia e sala em que cada exame ocorrerá. Deverá 
ser apresentado um aviso no terminal caso o número de salas e dias disponíveis sejam 
insuficientes para acomodar todos os exames necessários, sendo que não poderá ocorrer 
mais que um exame na mesma sala no mesmo dia (5 valores).-}

import Control.Monad (replicateM)
import Data.List (nub, sort)

type UC = String  -- Unidade Curricular
type Dia = String -- Data dos Exames
type Sala = String
type Distribuicao = [(UC, Dia, Sala)]

main :: IO ()
main = do
    putStrLn "Insira o número total de dias em que os exames podem ocorrer: "
    numDias <- getLine
    let dias = read numDias :: Int
    putStrLn ("São " ++ show dias ++ " dias de exames")

    putStrLn "Insira o número de salas disponíveis para exame: "
    numSalas <- getLine
    let salas = read numSalas :: Int
    putStrLn ("São " ++ show salas ++ " salas disponíveis para exame")

    putStrLn "Informe o número de exames a serem distribuídos: "
    numExames <- getLine
    let exames = read numExames :: Int
    putStrLn ("Serão distribuídos " ++ show exames ++ " exames")

    putStrLn "Informe a lotação de cada sala separada por espaços: "
    capacidades <- map read . words <$> getLine -- lê as capacidades das salas

    putStrLn "Informe os nomes das Unidades Curriculares (UCs):"
    ucs <- replicateM exames getLine -- lê as UCs a serem distribuídas

    let distribuicao = head (escalonarExames ucs capacidades dias salas) -- distribui as UCs em salas e dias
    let incompatibilidades = contarIncompatibilidades distribuicao -- conta as incompatibilidades

    putStrLn "Distribuição dos exames:"
    mapM_ printExame distribuicao -- imprime a distribuição dos exames

    putStrLn "Número total de incompatibilidades: "
    putStrLn (show incompatibilidades)

    -- Salvar a distribuição em um arquivo
    writeLines "distribuicao.txt" (map (\(uc, dia, sala) -> uc ++ "-" ++ dia ++ "-" ++ sala) distribuicao)
    putStrLn "Distribuição dos exames guardada com sucesso!"

readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path

writeLines :: FilePath -> [String] -> IO ()
writeLines path lines = writeFile path (unlines lines)

-- Distribui as UCs em salas e dias disponíveis
distribuirExames :: [UC] -> [Dia] -> [Int] -> Distribuicao
distribuirExames ucs dias capacidades
    | numExames > numSalas * numDias = error "Número insuficiente de salas e dias disponíveis."
    | otherwise = zipWith3 (\uc dia sala -> (uc, dia, sala)) ucs (cycle dias) (distribuirSalas capacidades)
    where
        numExames = length ucs
        numSalas = length capacidades
        numDias = length dias

-- Distribui as UCs em salas disponíveis
distribuirSalas :: [Int] -> [Sala]
distribuirSalas capacidades = concatMap replicateSalas capacidades

-- Repete as salas de acordo com a capacidade
replicateSalas :: Int -> [Sala]
replicateSalas capacidade = map (\n -> "Sala " ++ show n) [1..capacidade]

-- Imprime a distribuição de um exame
printExame :: (UC, Dia, Sala) -> IO ()
printExame (uc, dia, sala) = putStrLn $ uc ++ " - " ++ dia ++ " - " ++ sala

-- Conta o número de incompatibilidades na distribuição dos exames
contarIncompatibilidades :: Distribuicao -> Int
contarIncompatibilidades distribuicao =
    let paresUCs = combinations 2 (nub (map (\(uc, _, _) -> uc) distribuicao))
        incompatibilidades = map (\pair -> contarAlunosInscritos pair distribuicao) paresUCs
    in sum incompatibilidades

-- Distribui as UCs em uma única distribuição e verifica se há salas e dias suficientes
escalonarExames :: [UC] -> [Int] -> Int -> Int -> [Distribuicao]
escalonarExames ucs capacidades numDias numSalas =
    let distribuicao = distribuirExames ucs (take numDias (...cycle ["Dia " ++ show n | n <- [1..]]) -- cria uma lista com numDias dias
            capacidades -- usa a lista de capacidades de salas
        in if length distribuicao == length ucs -- verifica se todas as UCs foram distribuídas
            then [distribuicao]
            else escalonarExames ucs capacidades (numDias + 1) numSalas -- tenta aumentar o número de dias

-- Calcula todas as combinações possíveis de tamanho n em uma lista
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

-- Conta o número de alunos inscritos em ambas as UCs em um par de UCs
contarAlunosInscritos :: (UC, UC) -> Distribuicao -> Int
contarAlunosInscritos (uc1, uc2) distribuicao =
    let alunosUC1 = nub $ concatMap (\(uc, _, _) -> if uc == uc1 then ["aluno"] else []) distribuicao
        alunosUC2 = nub $ concatMap (\(uc, _, _) -> if uc == uc2 then ["aluno"] else []) distribuicao
    in length (alunosUC1 `intersect` alunosUC2) -- retorna a intersecção dos alunos inscritos nas duas UCs

-- Lê os dados das UCs de um arquivo de texto e imprime na tela
leucs :: FilePath -> IO ()
leucs path = do
    ucs <- readLines path -- lê as linhas do arquivo
    putStrLn "Unidades Curriculares (UCs):"
    mapM_ percorreuc ucs -- chama a função percorreuc para cada UC

-- Lê os dados dos alunos de um arquivo de texto e imprime na tela as UCs para cada aluno
percorrealunos :: FilePath -> FilePath -> IO ()
percorrealunos ucsPath alunosPath = do
    ucs <- readLines ucsPath   -- lê as UCs
    alunos <- readLines alunosPath -- lê os alunos
    putStrLn "Alunos:"
    mapM_ (\aluno -> do
        putStrLn ("Nome: " ++ aluno)
        putStrLn "Unidades Curriculares (UCs):"
        mapM_ (leinsc aluno) ucs) alunos -- chama a função leinsc para cada UC de cada aluno

-- Lê os dados dos alunos de um arquivo de texto e imprime na tela os alunos inscritos em cada UC
percorreinsc :: FilePath -> FilePath -> IO ()
percorreinsc ucsPath alunosPath = do
    ucs <- readLines ucsPath   -- lê as UCs
    alunos <- readLines alunosPath -- lê os alunos
    putStrLn "Unidades Curriculares (UCs):"
    mapM_ (\uc -> do
        putStrLn ("UC: " ++ uc)
        putStrLn "Alunos inscritos:"
        mapM_ (leinsc uc) alunos) ucs -- chama a função leinsc para cada aluno de cada UC

-- Lê as inscrições para uma UC de um arquivo de texto e imprime na tela
leinsc :: UC -> String -> IO ()
leinsc uc inscricoes = do
    let inscricoesUC = words inscricoes
    if uc `elem` inscricoesUC -- verifica se a UC está na lista de inscrições
        then putStrLn (" - " ++ inscricoes)
        else return ()

-- Imprime o nome completo de uma UC
percorreuc :: UC -> IO ()
percorreuc uc = putStrLn (" - " ++ uc)

