--
import System.IO ()
import Data.List (nub, sort)

type Incompatibility = (Int, Int, Int)  -- (UC1, UC2, incompatibilidade)

-- Função principal que lê os dados do usuário, calcula as incompatibilidades entre as unidades curriculares,
-- minimiza as incompatibilidades e escreve o escalonamento em um arquivo
tarefa5 :: IO ()
tarefa5 = do
  putStrLn "Informe o número de dias disponíveis:"
  ndias <- readLn -- Lê o número de dias disponíveis a partir da entrada do usuário
  putStrLn "Informe o número de salas disponíveis:"
  nsalas <- readLn -- Lê o número de salas disponíveis a partir da entrada do usuário
  ucs <- readFile "ucs.txt" -- Lê as unidades curriculares a partir do arquivo "ucs.txt"
  let nUCS = length (lines ucs) -- Calcula o número de unidades curriculares
  if nUCS > ndias * nsalas -- Verifica se é possível acomodar todas as unidades curriculares com os dias e salas disponíveis
    then
      putStrLn "Número de dias e salas insuficientes para acomodar todas as unidades curriculares."
  else 
    do
      inscricoes <- readInscricoes "inscricoes.txt" -- Lê as inscrições dos alunos a partir do arquivo "inscricoes.txt"
      let incompatibilities = calculateIncompatibilities inscricoes -- Calcula as incompatibilidades entre as unidades curriculares
          minimizedIncompatibilities = minimizeIncompatibilities incompatibilities -- Minimiza as incompatibilidades
          escalonamento = formatarDistribuicao (lines ucs) ndias nsalas minimizedIncompatibilities -- Formata o escalonamento
      writeFile "escalonamento.txt" escalonamento -- Escreve o escalonamento no arquivo "escalonamento.txt"
      putStrLn "A distribuição de exames foi concluída com sucesso."

-- Função auxiliar que lê as inscrições dos alunos a partir de um arquivo
readInscricoes :: String -> IO [(String, Int)]
readInscricoes file = map parseEntry <$> lines <$> readFile file
  where
    parseEntry :: String -> (String, Int)
    parseEntry entry = let [aluno, uc] = words entry in (aluno, read uc)

-- Função que calcula as incompatibilidades entre as unidades curriculares
calculateIncompatibilities :: [(String, Int)] -> [Incompatibility]
calculateIncompatibilities inscricoes =
  let ucs = sort $ nub $ map snd inscricoes -- Obtém a lista de unidades curriculares
      pairs = [(x, y) | x <- ucs, y <- ucs, x < y] -- Gera todas as combinações possíveis de pares de unidades curriculares
  in
     map (\(x, y) -> (x, y, countIncompatibleStudents x y inscricoes)) pairs -- Calcula o número de alunos incompatíveis para cada par de unidades curriculares

-- Função auxiliar que conta o número de alunos incompatíveis entre duas unidades curriculares
countIncompatibleStudents :: Int -> Int -> [(String, Int)] -> Int
countIncompatibleStudents uc1 uc2 inscricoes =
  let alunosUC1 = filter (\(_, uc) -> uc == uc1) inscricoes -- Filtra os alunos que se inscreveram na unidade curricular uc1
      alunosUC2 = filter (\(_, uc) -> uc == uc2) inscricoes -- Filtra os alunos que se inscreveram na unidade curricular uc2
      alunosComuns = length $ nub $ map fst $ filter (\(aluno, _) -> aluno `elem` map fst alunosUC2) alunosUC1 -- Conta o número de alunos em comum entre as duas unidades curriculares
  in 
    alunosComuns

-- Função que minimiza as incompatibilidades mantendo apenas as incompatibilidades necessárias
minimizeIncompatibilities :: [Incompatibility] -> [Incompatibility]
minimizeIncompatibilities incompatibilities = minimizeIncompatibilities' incompatibilities []
  where
    minimizeIncompatibilities' :: [Incompatibility] -> [Incompatibility] -> [Incompatibility]
    minimizeIncompatibilities' [] minimized = minimized -- Caso base: todas as incompatibilidades foram verificadas
    minimizeIncompatibilities' ((uc1, uc2, incomp):rest) minimized =
      if isCompatible uc1 uc2 minimized -- Verifica se o par de unidades curriculares é compatível com as incompatibilidades já encontradas
        then
          minimizeIncompatibilities' rest minimized -- Se for compatível, continua verificando as próximas incompatibilidades
        else
          minimizeIncompatibilities' rest ((uc1, uc2, incomp):minimized) -- Se não for compatível, adiciona a incompatibilidade à lista

    -- Função auxiliar que verifica se um par de unidades curriculares é compatível com as incompatibilidades já encontradas
    isCompatible :: Int -> Int -> [Incompatibility] -> Bool
    isCompatible uc1 uc2 incompatibilities = all (\(c1, c2, _) -> c1 /= uc1 || c2 /= uc2) incompatibilities

-- Função que formata o escalonamento dos exames
formatarDistribuicao :: [String] -> Int -> Int -> [Incompatibility] -> String
formatarDistribuicao ucs ndias nsalas incompatibilities =
  if ndias * nsalas < length ucs -- Verifica se há salas suficientes para acomodar todas as unidades curriculares
    then
      "Não há salas suficientes para acomodar todos os exames necessários."
    else
      unlines $ formatarDias 1 ucs ndias nsalas incompatibilities -- Formata os dias e as salas para o escalonamento

-- Função auxiliar que formata os dias e as salas para o escalonamento
formatarDias :: Int -> [String] -> Int -> Int -> [Incompatibility] -> [String]
formatarDias _ [] _ _ _ = [] -- Caso base: não há mais unidades curriculares para distribuir nos dias
formatarDias dia ucs ndias nsalas incompatibilities =
  let (ucsDia, ucsResto) = splitAt nsalas ucs -- Divide as unidades curriculares para o dia atual e o restante
  in ("Dia " ++ show dia) : formatarSalas 1 ucsDia incompatibilities ++ formatarDias (dia + 1) ucsResto ndias nsalas incompatibilities -- Formata as salas para o dia atual e continua com o próximo dia

-- Função auxiliar que formata as salas para o escalonamento
formatarSalas :: Int -> [String] -> [Incompatibility] -> [String]
formatarSalas _ [] _ = [] -- Caso base: não há mais unidades curriculares para distribuir nas salas
formatarSalas sala (uc:resto) incompatibilities =
  let ucNum = read (head $ words uc) -- Obtém o número da unidade curricular
      compatibleUCs = getCompatibleUCs ucNum incompatibilities -- Obtém as unidades curriculares compatíveis com a unidade curricular atual
  in ("  sala " ++ show sala ++ " - " ++ nomeUC uc ++ " (Compatível com: " ++ formatCompatibleUCs compatibleUCs ++ ")") : formatarSalas (sala + 1) resto incompatibilities -- Formata a sala atual e continua com a próxima sala

-- Função auxiliar que obtém as unidades curriculares compatíveis com uma unidade curricular específica
getCompatibleUCs :: Int -> [Incompatibility] -> [Int]
getCompatibleUCs uc incompatibilities = [c | (c1, c2, _) <- incompatibilities, c1 == uc || c2 == uc, c <- [c1, c2], c /= uc]

-- Função auxiliar que formata as unidades curriculares compatíveis como uma string
formatCompatibleUCs :: [Int] -> String
formatCompatibleUCs ucs = unwords $ map show ucs

-- Função auxiliar que obtém o nome da unidade curricular a partir de uma linha formatada
nomeUC :: String -> String
nomeUC = unwords . drop 2 . words
