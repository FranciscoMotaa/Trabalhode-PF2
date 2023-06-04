import System.IO
import Data.List (nub, sort)
import Data.Char(isSpace)

type Incompatibility = (Int, Int, Int)  -- (UC1, UC2, incompatibilidade)

tarefa5 :: IO ()
tarefa5 = do
  putStrLn "Informe o número de dias disponíveis:"
  numDias <- readLn
  putStrLn "Informe o número de salas disponíveis:"
  numSalas <- readLn
  ucs <- readFile "ucs.txt"
  let numUCs = length (lines ucs)
  if numUCs > numDias * numSalas then
    putStrLn "Número de dias e salas insuficientes para acomodar todas as unidades curriculares."
  else do
    inscricoes <- readInscricoes "inscricoes.txt"
    let incompatibilities = calculateIncompatibilities inscricoes
        minimizedIncompatibilities = minimizeIncompatibilities incompatibilities
        escalonamento = formatarDistribuicao (lines ucs) numDias numSalas minimizedIncompatibilities
    writeFile "escalonamento.txt" escalonamento
    putStrLn "Distribuição de Exames concluída."

readInscricoes :: String -> IO [(String, Int)]
readInscricoes file = map parseEntry <$> lines <$> readFile file
  where
    parseEntry :: String -> (String, Int)
    parseEntry entry =
        case words entry of
            [aluno, uc] -> (aluno, read uc)
            _ -> ("aluno desconhecido", 0)
        where
            trim :: String -> String
            trim = f . f
                where
                    f = reverse . dropWhile isSpace
calculateIncompatibilities :: [(String, Int)] -> [Incompatibility]
calculateIncompatibilities inscricoes =
  let ucs = sort $ nub $ map snd inscricoes
      pairs = [(x, y) | x <- ucs, y <- ucs, x < y]
  in map (\(x, y) -> (x, y, countIncompatibleStudents x y inscricoes)) pairs
  where
    countIncompatibleStudents :: Int -> Int -> [(String, Int)] -> Int
    countIncompatibleStudents uc1 uc2 inscricoes =
      let alunosUC1 = filter (\(_, uc) -> uc == uc1) inscricoes
          alunosUC2 = filter (\(_, uc) -> uc == uc2) inscricoes
          alunosComuns = length $ nub $ map fst $ filter (\(aluno, _) -> aluno `elem` map fst alunosUC2) alunosUC1
      in alunosComuns

minimizeIncompatibilities :: [Incompatibility] -> [Incompatibility]
minimizeIncompatibilities incompatibilities = minimizeIncompatibilities' incompatibilities []
  where
    minimizeIncompatibilities' :: [Incompatibility] -> [Incompatibility] -> [Incompatibility]
    minimizeIncompatibilities' [] minimized = minimized
    minimizeIncompatibilities' ((uc1, uc2, incomp):rest) minimized =
      if isCompatible uc1 uc2 minimized
        then minimizeIncompatibilities' rest minimized
        else minimizeIncompatibilities' rest ((uc1, uc2, incomp):minimized)

    isCompatible :: Int -> Int -> [Incompatibility] -> Bool
    isCompatible uc1 uc2 incompatibilities = all (\(c1, c2, _) -> c1 /= uc1 || c2 /= uc2) incompatibilities

formatarDistribuicao :: [String] -> Int -> Int -> [Incompatibility] -> String
formatarDistribuicao ucs numDias numSalas incompatibilities =
  if numDias * numSalas < length ucs
    then "Não há salas suficientes para acomodar todos os exames necessários."
    else unlines $ formatarDias 1 ucs numDias numSalas incompatibilities

formatarDias :: Int -> [String] -> Int -> Int -> [Incompatibility] -> [String]
formatarDias _ [] _ _ _ = []
formatarDias dia ucs numDias numSalas incompatibilities =
  let (ucsDia, ucsResto) = splitAt numSalas ucs
  in ("Dia " ++ show dia) : formatarSalas 1 ucsDia incompatibilities ++ formatarDias (dia + 1) ucsResto numDias numSalas incompatibilities

formatarSalas :: Int -> [String] -> [Incompatibility] -> [String]
formatarSalas _ [] _ = []
formatarSalas sala (uc:resto) incompatibilities =
  let ucNum = read (head $ words uc)
      compatibleUCs = getCompatibleUCs ucNum incompatibilities
  in ("  sala " ++ show sala ++ " - " ++ nomeUC uc ++ " (Compatível com: " ++ formatCompatibleUCs compatibleUCs ++ ")") : formatarSalas (sala + 1) resto incompatibilities

getCompatibleUCs :: Int -> [Incompatibility] -> [Int]
getCompatibleUCs uc incompatibilities = [c | (c1, c2, _) <- incompatibilities, c1 == uc || c2 == uc, c <- [c1, c2], c /= uc]

formatCompatibleUCs :: [Int] -> String
formatCompatibleUCs ucs = unwords $ map show ucs

nomeUC :: String -> String
nomeUC = unwords . drop 2 . words