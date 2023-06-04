import System.IO
tarefa1 :: IO ()
tarefa1 = do
  -- Solicita ao usuário a quantidade de dias disponíveis
  putStrLn "Por favor, indique a quantidade de dias que estão disponíveis para a realização dos exames:"
  ndiasstr <- getLine
  -- Solicita ao usuário o número de salas disponíveis
  putStrLn "Por favor, forneça o número de salas que estão disponíveis."
  nSalasstr <- getLine
  -- Converte as strings para valores numéricos
  let ndias = read ndiasstr :: Int
      nsalas = read nSalasstr :: Int
  -- Lê as unidades curriculares do arquivo "ucs.txt"
  ucs <- readFile "ucs.txt"
  -- Calcula o número de unidades curriculares
  let nucs = length (lines ucs)
  -- Verifica se há dias e salas suficientes para alocar todas as unidades curriculares
  if nucs > ndias * nsalas then
      putStrLn "Não há dias nem salas suficientes para alocar todas as unidades curriculares."
  else 
    do
      -- Formata a distribuição dos exames e cria um arquivo "escalonamento.txt"
      let escalonamento = formatadistri (lines ucs) ndias nsalas
      writeFile "escalonamento.txt" escalonamento
      putStrLn "A alocação dos exames foi finalizada com sucesso."

-- Formata a distribuição dos exames em dias e salas
formatadistri :: [String] -> Int -> Int -> String
formatadistri ucs ndias nsalas =
  if ndias * nsalas < length ucs 
    then
    "Não há salas suficientes para acomodar todos os exames necessários."
  else
    unlines (formatadia 1 ucs ndias nsalas)

-- Formata a distribuição dos exames em dias
formatadia :: Int -> [String] -> Int -> Int -> [String]
formatadia _ [] _ _  = []
formatadia dia ucs ndias nsalas =
    let 
      (ucsDia, ucsResto) = splitAt nsalas ucs
    in
       ("Dia " ++ show dia) : formatasala 1 ucsDia ++ formatadia (dia + 1) ucsResto ndias nsalas

-- Formata a distribuição dos exames em salas
formatasala :: Int -> [String] -> [String]
formatasala _ []= []
formatasala sala (x:xs) = (" sala " ++ show sala ++ " -> " ++ nomeUC x) : formatasala (sala + 1) xs

-- Extrai o nome da unidade curricular a partir da string
nomeUC :: String -> String
nomeUC = unwords . drop 2 . words