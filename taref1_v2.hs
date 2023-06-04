import System.IO()
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
    capacidades <- map read . words <$> getLine

    putStrLn "Informe os nomes das Unidades Curriculares (UCs):"
    ucs <- replicateM exames getLine

    let distribuicao = head (escalonarExames ucs capacidades dias salas)
    let incompatibilidades = contarIncompatibilidades distribuicao

    putStrLn "Distribuição dos exames:"
    mapM_ printExame distribuicao

    putStrLn "Número total de incompatibilidades: "
    putStrLn (show incompatibilidades)

    -- Salvar a distribuição em um arquivo
    writeLines "distribuicao.txt" (map (\(uc, dia, sala) -> uc ++ "-" ++ dia ++ "-" ++ sala) distribuicao)
    putStrLn "Distribuição dos exames guardada com sucesso!"

readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path

writeLines :: FilePath -> [String] -> IO ()
writeLines path lines = writeFile path (unlines lines)

distribuirExames :: [UC] -> [Dia] -> [Int] -> Distribuicao
distribuirExames ucs dias capacidades
    | numExames > numSalas * numDias = error "Número insuficiente de salas e dias disponíveis."
    | otherwise = zipWith3 (\uc dia sala -> (uc, dia, sala)) ucs (cycle dias) (distribuirSalas capacidades)
    where
        numExames = length ucs
        numSalas = length capacidades
        numDias = length dias

distribuirSalas :: [Int] -> [Sala]
distribuirSalas capacidades = concatMap replicateSalas capacidades

replicateSalas :: Int -> [Sala]
replicateSalas capacidade = map (\n -> "Sala " ++ show n) [1..capacidade]

printExame :: (UC, Dia, Sala) -> IO ()
printExame (uc, dia, sala) = putStrLn $ uc ++ " - " ++ dia ++ " - " ++ sala

contarIncompatibilidades :: Distribuicao -> Int
contarIncompatibilidades distribuicao =
    let paresUCs = combinations 2 (nub (map (\(uc, _, _) -> uc) distribuicao))
        incompatibilidades = map (\pair -> contarAlunosInscritos pair distribuicao) paresUCs
    in sum incompatibilidades

escalonarExames :: [UC] -> [Int] -> Int -> Int -> [Distribuicao]
escalonarExames ucs capacidades numDias numSalas =
    let distribuicao = distribuirExames ucs (take numDias (repeat "Dia")) capacidades
    in if length distribuicao < length ucs
           then error "Número insuficiente de salas e dias disponíveis."
           else [distribuicao]

contarAlunosInscritos :: [UC] -> Distribuicao -> Int
contarAlunosInscritos ucs distribuicao =
    let alunosUCs = map (\uc -> contarAlunosUC uc distribuicao) ucs
    in minimum alunosUCs

contarAlunosUC :: UC -> Distribuicao -> Int
contarAlunosUC uc distribuicao = length (filter (\(uc', _, _) -> uc == uc') distribuicao)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs
--A função leucs imprime na tela a lista de UCs e para cada UC imprime os alunos inscritos nela usando a função leinsc. 
leucs :: [String] -> String -> IO()
leucs [] _ = putStrLn ("")
leucs (linha:linhas) inscricoes = do
                        putStrLn (unwords(tail(tail(words linha))))
                        leinsc (head(words linha)) (lines inscricoes)
                        leucs linhas inscricoes

-- A função leinsc recebe uma UC e uma lista de inscrições e imprime na tela os alunos que estão inscritos na UC. 
leinsc :: String -> [String] -> IO()
leinsc _ [] = putStrLn ("")
leinsc uc (linha:linhas) = if uc == last(words linha) then do alunos <- readFile "listaalunos.txt"
                                                              mapalunos (head(words linha)) (lines alunos)
                                                              leinsc uc linhas
                                else leinsc uc linhas
--A função mapalunos recebe um aluno e uma lista de alunos e imprime na tela o nome completo do aluno. 
mapalunos :: String -> [String] -> IO()
mapalunos _ [] = putStrLn ("")
mapalunos al (linha:linhas) = if al == head(words linha) then do putStrLn (unwords(tail(tail(words linha))))
                                    else mapalunos al linhas


{-A função percorrealunos ela imprime a lista de UCs para cada aluno -- respectivamente. A função percorreuc recebe uma UC e uma lista de UCs e imprime na tela o nome completo da UC.-}
percorrealunos :: [String] -> String -> IO()
percorrealunos [] _ = putStrLn ("")
percorrealunos (linha:linhas) inscricoes = do
                            putStrLn (unwords(tail(tail(words linha))))
                            percorreinsc (head(words linha)) (lines inscricoes)
                            percorrealunos linhas inscricoes

{-A função percorreinsc ela imprime  lista de alunos para cada UC. -}
percorreinsc :: String -> [String] -> IO()
percorreinsc _ [] = putStrLn ("")
percorreinsc al (linha:linhas) = if al == head(words linha) then do ucs <- readFile "ucs.txt"
                                                                    percorreuc (last(words linha)) (lines ucs)
                                                                    percorreinsc al linhas
                                else percorreinsc al linhas
--A função percorreuc recebe uma UC e uma lista de UCs e imprime na tela o nome completo da UC.
percorreuc :: String -> [String] -> IO()
percorreuc _ [] = putStrLn ("")
percorreuc uc (linha:linhas) = if uc == head(words linha) then do putStrLn (unwords(tail(tail(words linha))))
                                else percorreuc uc linhas