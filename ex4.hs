{----------} 
import System.IO ()

-- Função principal que realiza a tarefa 4
tarefa4 :: IO ()
tarefa4 = do
  putStrLn "Informe o número de dias disponíveis:"
  ndias <- readLn
  putStrLn "Informe o número de salas disponíveis:"
  nsalas <- readLn
  ucs <- readFile "ucs.txt"
  let numUCs = length (lines ucs)

  if numUCs > ndias * nsalas -- Verifica se há dias e salas suficientes para acomodar todas as unidades curriculares
    then
      putStrLn "Número de dias e salas insuficientes para acomodar todas as unidades curriculares."
  else
    do
      let distribuicao = formatarDistribuicao (lines ucs) ndias nsalas -- Formata a distribuição dos exames
      writeFile "distribuicao.txt" distribuicao -- Escreve a distribuição no arquivo "distribuicao.txt"
      let totalIncompatibilidades = contarTotalIncompatibilidades distribuicao -- Conta o total de incompatibilidades na distribuição
      appendFile "distribuicao.txt" ("\nTotal de Incompatibilidades: " ++ show totalIncompatibilidades) -- Adiciona o total de incompatibilidades ao final do arquivo "distribuicao.txt"

      putStrLn "Distribuição de Exames concluída."

-- Função que formata a distribuição dos exames
formatarDistribuicao :: [String] -> Int -> Int -> String
formatarDistribuicao ucs ndias nsalas =
  if ndias * nsalas < length ucs -- Verifica se há salas suficientes para acomodar todas as unidades curriculares
    then
      "Não há salas suficientes para acomodar todos os exames necessários."
    else
      unlines $ formatarDias 1 ucs ndias nsalas -- Formata os dias e as salas para a distribuição

-- Função auxiliar que formata os dias e as salas para a distribuição
formatarDias :: Int -> [String] -> Int -> Int -> [String]
formatarDias _ [] _ _ = [] -- Caso base: não há mais unidades curriculares para distribuir nos dias
formatarDias dia ucs ndias nsalas =
  let (ucsDia, ucsResto) = splitAt nsalas ucs -- Divide as unidades curriculares para o dia atual e o restante
  in
    ("Dia " ++ show dia) : formatarSalas 1 ucsDia ++ formatarDias (dia + 1) ucsResto ndias nsalas -- Formata o dia atual, as salas e continua com o próximo dia

-- Função auxiliar que formata as salas para a distribuição
formatarSalas :: Int -> [String] -> [String]
formatarSalas _ [] = [] -- Caso base: não há mais unidades curriculares para distribuir nas salas
formatarSalas sala (uc:resto) = ("  sala " ++ show sala ++ " - " ++ nomeUC uc) : formatarSalas (sala + 1) resto -- Formata a sala atual e continua com a próxima sala

-- Função que conta o total de incompatibilidades na distribuição
contarTotalIncompatibilidades :: String -> Int
contarTotalIncompatibilidades distribuicao =
  let linhas = lines distribuicao
      incompatibilidades = filter (\linha -> take 4 linha == "  sala") linhas -- Filtra as linhas que representam as incompatibilidades
  in
    length incompatibilidades -- Retorna o total de incompatibilidades

-- Função auxiliar que obtém o nome da unidade curricular a partir de uma linha formatada
nomeUC :: String -> String
nomeUC = unwords . drop 2 . words