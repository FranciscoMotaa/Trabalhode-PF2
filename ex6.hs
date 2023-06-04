import System.IO()

tarefa6 :: IO ()
tarefa6 = do
  putStrLn "Informe o número de dias disponíveis:"
  numDias <- readLn
  putStrLn "Informe o número de salas disponíveis:"
  numSalas <- readLn
  putStrLn "Informe a lotação máxima de cada sala:"
  lotacaoSala <- readLn
  ucs <- readFile "ucs.txt"
  let numUCs = length (lines ucs)
  if numUCs > numDias * numSalas then
      putStrLn "Número de dias e salas insuficientes para acomodar todas as unidades curriculares."
  else do
      let distribuicao = formatarDistribuicao (lines ucs) numDias numSalas lotacaoSala
      writeFile "distribuicao.txt" distribuicao
      putStrLn "Distribuição de Exames concluída."

formatarDistribuicao :: [String] -> Int -> Int -> Int -> String
formatarDistribuicao ucs numDias numSalas lotacaoSala =
  if numDias * numSalas < length ucs
    then "Não há salas suficientes para acomodar todos os exames necessários."
    else unlines $ formatarDias 1 ucs numDias numSalas lotacaoSala

formatarDias :: Int -> [String] -> Int -> Int -> Int -> [String]
formatarDias _  [] _ _ _ =  [] 
formatarDias dia ucs numDias numSalas lotacaoSala =
  let (ucsDia, ucsResto) = splitAt numSalas ucs
  in ("Dia " ++ show dia) : formatarSalas 1 ucsDia lotacaoSala ++ formatarDias (dia + 1) ucsResto numDias numSalas lotacaoSala

formatarSalas :: Int -> [String] -> Int -> [String]
formatarSalas _ [] _ = []
formatarSalas sala (uc:resto) lotacaoSala =
  let numAlunos = read (words uc !! 1) :: Int
      numSalasNecessarias = ceiling (fromIntegral numAlunos / fromIntegral lotacaoSala)
  in if numSalasNecessarias <= 1
      then ("  sala " ++ show sala ++ " - " ++ nomeUC uc) : formatarSalas (sala + 1) resto lotacaoSala
      else concatMap (\s -> ["  sala " ++ show s ++ " - " ++ nomeUC uc]) [sala..(sala+numSalasNecessarias-1)] ++ formatarSalas (sala + numSalasNecessarias) resto lotacaoSala


nomeUC :: String -> String
nomeUC = unwords . drop 2 . words