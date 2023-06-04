import Data.List (nub, sort, subsequences)
import Data.Char (isSpace) -- Importar a função isSpace para lidar com espaços em branco

import System.IO

tarefa4 :: IO ()
tarefa4 = do
  putStrLn "Informe o número de dias disponíveis:"
  numDias <- readLn
  putStrLn "Informe o número de salas disponíveis:"
  numSalas <- readLn
  ucs <- readFile "ucs.txt"
  let numUCs = length (lines ucs)
  if numUCs > numDias * numSalas then
      putStrLn "Número de dias e salas insuficientes para acomodar todas as unidades curriculares."
  else do
      let distribuicao = formatarDistribuicao (lines ucs) numDias numSalas
      writeFile "distribuicao.txt" distribuicao
      let totalIncompatibilidades = contarTotalIncompatibilidades distribuicao
      appendFile "distribuicao.txt" ("\nTotal de Incompatibilidades: " ++ show totalIncompatibilidades)

      putStrLn "Distribuição de Exames concluída."

formatarDistribuicao :: [String] -> Int -> Int -> String
formatarDistribuicao ucs numDias numSalas =
  if numDias * numSalas < length ucs
    then "Não há salas suficientes para acomodar todos os exames necessários."
    else unlines $ formatarDias 1 ucs numDias numSalas

formatarDias :: Int -> [String] -> Int -> Int -> [String]
formatarDias _ [] _ _ =  [] 
formatarDias dia ucs numDias numSalas =
  let (ucsDia, ucsResto) = splitAt numSalas ucs
  in ("Dia " ++ show dia) : formatarSalas 1 ucsDia ++ formatarDias (dia + 1) ucsResto numDias numSalas


formatarSalas :: Int -> [String] -> [String]
formatarSalas _ [] = []
formatarSalas sala (uc:resto) = ("  sala " ++ show sala ++ " - " ++ nomeUC uc) : formatarSalas (sala + 1) resto


contarTotalIncompatibilidades :: String -> Int
contarTotalIncompatibilidades distribuicao =
  let linhas = lines distribuicao
      incompatibilidades = filter (\linha -> take 4 linha == "  sala") linhas
  in length incompatibilidades


nomeUC :: String -> String
nomeUC = unwords . drop 2 . words
