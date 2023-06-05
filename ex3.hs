{--}
import Data.List (nub, sort, subsequences)
import System.IO()
import Control.Exception (catch, IOException)
import Data.Char(isSpace)

-- Função que lê as inscrições do arquivo e retorna uma lista de tuplas (aluno, UC)
lerinscricoes :: String -> IO [(String, Int)]
lerinscricoes ficheiro = do
  conteudo <- readFile ficheiro
  let inscricoes = lines conteudo
  return $ map analiseentrada inscricoes
  where
    -- Função auxiliar para analisar cada linha do arquivo e criar uma tupla (aluno, UC)
    analiseentrada :: String -> (String, Int)
    analiseentrada entrada =
        case words entrada of
            [aluno, uc] -> (aluno, read uc)
            _ -> ("aluno desconhecido", 0)
        where
            -- Função auxiliar para remover espaços em branco no início e no fim de uma string
            trim :: String -> String
            trim = f . f
                where
                    f = reverse . dropWhile isSpace

-- Função que calcula as incompatibilidades entre cada par de UCs
calcularincompatibilidades :: [(String, Int)] -> [(Int, Int, Int)]
calcularincompatibilidades inscricoes =
  let ucs = sort $ nub $ map snd inscricoes -- Obtém a lista de UCs sem repetições e em ordem crescente
      pairs = [(x, y) | x <- ucs, y <- ucs, x < y] -- Cria todas as combinações possíveis de pares de UCs
  in 
    map (\(x, y) -> (x, y, contarIncompatibilidades x y inscricoes)) pairs
  where
    -- Função auxiliar para contar o número de alunos incompatíveis entre duas UCs
    contarIncompatibilidades :: Int -> Int -> [(String, Int)] -> Int
    contarIncompatibilidades uc1 uc2 inscricoes =
      let alunosUC1 = filter (\(_, uc) -> uc == uc1) inscricoes -- Filtra os alunos inscritos na UC 1
          alunosUC2 = filter (\(_, uc) -> uc == uc2) inscricoes -- Filtra os alunos inscritos na UC 2
          alunosEmComum = length $ nub $ map fst $ filter (\(aluno, _) -> aluno `elem` map fst alunosUC2) alunosUC1 -- Conta os alunos em comum
      in 
        alunosEmComum

-- Função que imprime as incompatibilidades na tela
printIncompatibilidades :: [(Int, Int, Int)] -> IO ()
printIncompatibilidades imcompatibilidades = do
  putStrLn "Conflitos entre combinações de unidades curriculares:"
  mapM_ (\(uc1, uc2, incompatibilidade) -> putStrLn $ "UC " ++ show uc1 ++ " e UC " ++ show uc2 ++ ": " ++ show incompatibilidade) imcompatibilidades

main :: IO ()
main = do
  inscricoes <- lerinscricoes "inscricoes.txt" -- Lê as inscrições do arquivo
  let imcompatibilidades = calcularincompatibilidades inscricoes -- Calcula as incompatibilidades entre as UCs
  printIncompatibilidades imcompatibilidades -- Imprime as incompatibilidades
