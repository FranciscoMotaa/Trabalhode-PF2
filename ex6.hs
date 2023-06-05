import System.IO()

-- Função principal que lê os dados do usuário, verifica se é possível acomodar todas as unidades curriculares,
-- e, se for o caso, escreve a distribuição de exames em um arquivo
tarefa6 :: IO ()
tarefa6 = do
  putStrLn "Por favor, informe a quantidade de dias disponíveis:"
  ndias <- readLn -- Lê a quantidade de dias disponíveis a partir da entrada do usuário
  putStrLn "Por favor, informe a quantidade de salas disponíveis:"
  nsalas <- readLn -- Lê a quantidade de salas disponíveis a partir da entrada do usuário
  putStrLn "Por favor, informe a quantidade máxima de pessoas que podem ser acomodadas em cada sala:"
  lotacaoSala <- readLn -- Lê a lotação máxima de cada sala a partir da entrada do usuário
  ucs <- readFile "ucs.txt" -- Lê as unidades curriculares a partir do arquivo "ucs.txt"
  let nucs = length (lines ucs) -- Calcula o número de unidades curriculares
  if nucs > ndias * nsalas -- Verifica se é possível acomodar todas as unidades curriculares com as salas e dias disponíveis
    then
      putStrLn "Devido à limitação de dias e salas disponíveis, não é possível acomodar todas as unidades curriculares."
  else 
    do
      let distribuicao = formatadistri (lines ucs) ndias nsalas lotacaoSala -- Formata a distribuição de exames
      writeFile "distribuicao.txt" distribuicao -- Escreve a distribuição de exames no arquivo "distribuicao.txt"
      putStrLn "Distribuição de Exames concluída."

-- Função que formata a distribuição de exames
formatadistri :: [String] -> Int -> Int -> Int -> String
formatadistri ucs ndias nsalas lotacaoSala =
  if ndias * nsalas < length ucs -- Verifica se há salas suficientes para acomodar todas as unidades curriculares
    then
       "Não há salas suficientes para acomodar todos os exames necessários."
    else
       unlines $ formatadia 1 ucs ndias nsalas lotacaoSala -- Formata a distribuição de exames por dia

-- Função que formata a distribuição de exames por dia
formatadia :: Int -> [String] -> Int -> Int -> Int -> [String]
formatadia _ [] _ _ _ =  [] -- Caso base: não há mais unidades curriculares para distribuir
formatadia dia ucs ndias nsalas lotacaoSala =
  let
     (ucsDia, ucsResto) = splitAt nsalas ucs -- Divide a lista de unidades curriculares em sublistas de tamanho nsalas
  in 
    ("Dia " ++ show dia) : formatasalas 1 ucsDia lotacaoSala ++ formatadia (dia + 1) ucsResto ndias nsalas lotacaoSala -- Formata a distribuição de exames para o dia atual e chama a função recursivamente para o próximo dia

-- Função que formata a distribuição de exames por sala
formatasalas :: Int -> [String] -> Int -> [String]
formatasalas _ [] _ = [] -- Caso base: não há mais unidades curriculares para distribuir na sala atual
formatasalas sala (uc:resto) lotacaoSala =
  let numAlunos = read (words uc !! 1) :: Int -- Lê o número de alunos da unidade curricular atual
      nsalasnecess = ceiling (fromIntegral numAlunos / fromIntegral lotacaoSala) -- Calcula o número de salas necessárias para acomodar todos os alunos da unidade curricular
  in
     if 
      nsalasnecess <= 1 -- Se a unidade curricular pode ser acomodada em apenas uma sala, formata a distribuição de exames para essa sala
      then
         ("  sala " ++ show sala ++ " - " ++ nomeUC uc) : formatasalas (sala + 1) resto lotacaoSala
      else -- Se a unidade curricular precisa ser acomodada em mais de uma sala, formata a distribuição de exames para cada sala
         concatMap (\s -> ["  sala " ++ show s ++ " -> " ++ nomeUC uc]) [sala..(sala+ nsalasnecess-1)] ++ formatasalas (sala + nsalasnecess) resto lotacaoSala

-- Função auxiliar que retorna o nome da unidade curricular a partir de uma string no formato "codigo nome numAlunos"
nomeUC :: String -> String
nomeUC = unwords . drop 2 . words
