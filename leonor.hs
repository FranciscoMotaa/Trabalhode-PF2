import System.IO
import Data.List (nub, delete)

--tarefa1
exames:: IO()
exames = do 
    ucs<- readFile "ucs.txt"
    ucs1 <- openFile "Distribuicao.txt" WriteMode
    hClose ucs1 --fecha o arquivo

    --Pedir input
    putStrLn "indicate number of days in which the exam can take place"
    dias <- getLine
    putStrLn "indicate the number of classrooms available per day"
    salas <- getLine
    
    {-calcula o número de disciplinas (n_disciplinas) com base no número de linhas no arquivo ucs-}
    
    let n_disciplinas = length (lines ucs) 
    --lê o valor de dias e o converte para um tipo inteiro (Int)
    let n_dias = read dias :: Int
    
    --lê o valor de salas e o converte para um tipo inteiro (Int)
    let n_salas = read salas :: Int
    if n_disciplinas > n_dias * n_salas  -- se numero de disciplinas maior que dias livres * salas impossivel
        then putStrLn "Insufficient days to accommodate all exams"
        else distribuir 1 n_salas (lines ucs)-- 1 indica em que linha deve comecar a distribuicao


distribuir :: Int -> Int -> [String] -> IO() 
distribuir dias salas [] = return()
distribuir dias salas (x:xs) = do
    exames <- openFile "exames.txt" AppendMode
    hPutStrLn exames ("--- day "++ show dias ++ "---"){- hPutStrLn escreve uma string seguida de uma nova linha
    no aquivo. Ela recebe dois argumentos: o primeiro é o identificador do arquivo no qual será escrita a linha, e o segundo é a string que será escrita.-}
    hClose exames
    printSalas salas (x:xs)
    distribuir (dias+1) salas (funcTail salas (x:xs))
    {-(dias+1) é uma expressão que representa o valor do próximo dia.Na variável dias incrementa-se em 1 para indicar o próximo dia-}

{-retorna uma sublista da lista original comecando na posição sala
Ela utiliza recursão para remover os elementos da lista até que salas seja igual 
a 0 ou até que a lista seja completamente percorrida.-}
funcTail :: Int -> [String] -> [String]
funcTail salas [] = []
funcTail 0 string = string
funcTail salas (x:xs) = do
    funcTail (salas-1) xs

    
printSalas :: Int -> [String] -> IO()
printSalas salas [] = return()
printSalas 0 (x:xs) = return()
printSalas salas (x:xs) = do
    exames <- openFile "exames.txt" AppendMode
    hPutStrLn exames ("Classroom "++ show salas ++ ": "++ unwords (tail (tail(words x))))
    hClose exames
    printSalas (salas-1) xs




--tarefa2
solucaodistribuicao :: IO()
solucaodistribuicao  = do 
    conteudoDisciplina <- readFile "ucs.txt"

    -- Limpa o conteúdo do arquivo "solucao.txt"
    ficheiro <- openFile "solucao.txt" WriteMode
    hClose ficheiro

    -- Fazer uma cópia do conteúdo de "ucs.txt" para "Suporte2.txt"
    conteudoUCS <- readFile "ucs.txt"
    ficheiro2 <- openFile "Suporte2.txt" WriteMode
    hPutStr ficheiro2 conteudoUCS
    hClose ficheiro2

    -- Criar o arquivo "Suporte3.txt" vazio
    ficheiro3 <- openFile "Suporte3.txt" WriteMode
    hClose ficheiro3

    -- Criar o arquivo "Suporte.txt" e escrever "0" nele
    ficheiro <- openFile "Suporte.txt" WriteMode
    hPutStrLn ficheiro "0"
    hClose ficheiro

    
    --descobrir qual o ano mais alto escrever no ficheiro 1

    copyFile (lines conteudoDisciplina)

    suporte2 <- readFile' "Suporte2.txt"
    maiorAno (lines suporte2)
    let suporte = readFile' "Suporte.txt"
    maiorAnoString <- suporte
    let maiorAnoInt = read maiorAnoString :: Int -- maior ano no ficheiro
    

    --Pedir input
    putStrLn "indicate number of days in which the exam can take place"
    dias <- getLine
    putStrLn "indicate the number of rooms available per day"
    salas <- getLine
    --linhas no ficheiro-
    let n_disciplinas = length (lines conteudoDisciplina) -- numero de disciplinas é o numero de linhas no ficheiro
    --convercao-- para int
    let n_dias = read dias :: Int
    let n_salas = read salas :: Int

    distribuir2 maiorAnoInt 1 n_dias n_salas (lines conteudoDisciplina)

    suporte2 <- readFile' "Suporte2.txt"

    if null (lines suporte2)
        then return()
        else do 
            putStrLn "Insufficient days to accommodate all exams"
            return()



--funcoes tarefa 2 
maiorAno :: [String] -> IO()
maiorAno [] = return()
maiorAno (x:xs) = do 

    let suporte = readFile' "Suporte.txt"
    
    suporteString <-  suporte
    let suporteInt = read suporteString :: Int

    let numero = head(tail(words x))
    let numeroInt = read numero :: Int

    if numeroInt > suporteInt
        then do 
            ficheiro <- openFile "Suporte.txt" WriteMode
            hPrint ficheiro numeroInt
            hClose ficheiro
            maiorAno xs
        else maiorAno xs
            
distribuir2 :: Int -> Int -> Int -> Int -> [String] -> IO() 
distribuir2 anoMax dias diasMax salas [] = return()
distribuir2 anoMax dias diasMax salas (x:xs) = do

    if dias > diasMax
        then return()
        else do 

            ficheiro <- openFile "solucao.txt" AppendMode
            hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
            hClose ficheiro

            ficheiro <- openFile "Suporte3.txt" WriteMode
            hClose ficheiro --limpar ficheiro3

            --funcao que seleciona disciplinas para ficheiro3 e apaga o ficheiro 2
            repeater2 anoMax salas
            
            --ficheiro3 a zero

            suporte3 <- readFile' "Suporte3.txt"
            printSalas2 salas (lines suporte3)--print nos ficheiros do 3

            suporte2 <- readFile' "Suporte2.txt"
            if null (lines suporte2)
                then return()
                else do
                    ficheiro <- openFile "Suporte.txt" WriteMode
                    hPutStrLn ficheiro "0"
                    hClose ficheiro
                    maiorAno (lines suporte2)
                    let suporte = readFile' "Suporte.txt"
                    maiorAnoString <- suporte
                    let maiorAnoInt = read maiorAnoString :: Int -- maior ano no ficheiro
                    distribuir2 maiorAnoInt (dias+1) diasMax salas (lines suporte2)

finder :: Int -> [String] -> IO()
finder _ [] = return()
finder ano (x:xs) = do --ano a encontrar, ficheiro2
    let anoLinhaString = head(tail(words x))
    let anoLinhaInt = read anoLinhaString :: Int

    if ano == anoLinhaInt
        then do
            ficheiro <- openFile "Suporte3.txt" AppendMode
            hPutStrLn ficheiro x
            hClose ficheiro --limpar ficheiro3
            copyFile xs
            return()
        else do
            ficheiro <- openFile "Suporte2.txt" AppendMode
            hPutStrLn ficheiro x
            hClose ficheiro 
            finder ano xs

 
repeater2 :: Int -> Int -> IO()
repeater2 0 _ = return()
repeater2 _ 0 = return()
repeater2 anoMax salas = do --ano maximo a procurar e numero de salas a preencher
    suporte2 <- readFile' "Suporte2.txt"

    ficheiro <- openFile "Suporte2.txt" WriteMode
    hClose ficheiro --limpar ficheiro2

    finder anoMax (lines suporte2)
    repeater2 (anoMax-1)(salas-1)


printSalas2 :: Int -> [String] -> IO()
printSalas2 salas [] = return()
printSalas2 0 (x:xs) = return()
printSalas2 salas (x:xs) = do
    ficheiro <- openFile "solucao.txt" AppendMode
    hPutStrLn ficheiro ("Sala "++ show salas ++ ": "++ unwords (tail (tail(words x))))
    hClose ficheiro
    printSalas2 (salas-1) xs


-- tarefa 3
tarefa3 :: IO()
tarefa3 = do 
   tarefa3_1
   ficheiroSuporte <- readFile "Suporte.txt"
   let tamanhoInicial = length (lines ficheiroSuporte) 
   let tamanhoFinal = length (remDup(lines ficheiroSuporte)) 
   let incompativeis = tamanhoInicial - tamanhoFinal
   print incompativeis

--Funcoes tarefa 3--

remDup :: Eq a => [a] -> [a]
remDup [] = []
remDup (x:xs)
  | x `elem` xs = x : remDup (delete x xs)
  | otherwise = x : remDup xs

tarefa3_1 :: IO()
tarefa3_1 = do
    ficheiro <- openFile "Suporte.txt" WriteMode
    hClose ficheiro
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"
    putStrLn "indicate the number of the course"
    disciplina <- getLine
    encontrarNome disciplina (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)
    putStrLn "indicate the numer of another course"
    disciplina <- getLine
    encontrarNome disciplina (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)


encontrarNome :: String -> [String] -> [String] -> [String] -> IO() -- Find the discipline number from its name
encontrarNome x [] _ _ = return()
encontrarNome input (linha:linhas) conteudo_insc conteudo_alunos = do
    let numero = head (words linha)
    if input == unwords (tail (tail (words linha)))
        then encontrarAl numero conteudo_insc conteudo_alunos -- Use the number to find the student
        else return()
    encontrarNome input linhas conteudo_insc conteudo_alunos

encontrarAl :: String -> [String] -> [String] -> IO() -- Find the student's registration number from the discipline number
encontrarAl _ [] _ = return()
encontrarAl numero (linha:linhas) conteudo_alunos = do
    let numero_al = head (words linha) -- Student's registration number
    if last (words linha) == numero
        then do
            ficheiro <- openFile "Suporte.txt" AppendMode
            hPutStrLn ficheiro (head (words linha)) -- Convert registration number to name
            hClose ficheiro
        else return()
    encontrarAl numero linhas conteudo_alunos
--tarefa4
tarefa4 :: IO()
tarefa4 = do 
    --read 
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"
    --limpar ficheiros
    conteudoDisciplina <- readFile "ucs.txt"
    ficheiro <- openFile "Tarefa1.txt" WriteMode
    hClose ficheiro

    --Pedir input
    putStrLn "Indicate the number of days the exam can take place:"
    dias <- getLine
    putStrLn "Indicate the number of rooms available per day:"
    salas <- getLine
    --linhas no ficheiro-
    let n_disciplinas = length (lines conteudoDisciplina) -- numero de disciplinas é o numero de linhas no ficheiro
    --convercao-- para int
    let n_dias = read dias :: Int
    let n_salas = read salas :: Int
    if n_disciplinas > n_dias * n_salas  -- se numero de disciplinas maior que dias livres * salas impossivel
        then putStrLn "Insufficient days to accommodate all exams"
        else distribuir4 (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos) 1 n_salas (lines conteudoDisciplina)


distribuir4 :: [String] -> [String] -> [String] -> Int -> Int -> [String] -> IO()
distribuir4 disciplina inscricao alunos dias salas [] = return()
distribuir4 disciplina inscricao alunos dias salas (x:xs) = do
    writeFile "Suporte.txt" ""

    appendFile "exames.txt" ("--- dia " ++ show dias ++ "---\n")

    printSalas4 disciplina inscricao alunos salas (x:xs)

    ficheiroSuporte <- readFile "Suporte.txt"
    let tamanhoInicial = length (lines ficheiroSuporte)
    let tamanhoFinal = length (remDup (lines ficheiroSuporte))
    let incompativeis = tamanhoInicial - tamanhoFinal

    appendFile "exames.txt" ("Number of incompatibilities: " ++ show incompativeis ++ "\n")

    distribuir4 disciplina inscricao alunos (dias + 1) salas (funcTail salas (x:xs))

printSalas4 :: [String] -> [String] -> [String] -> Int -> [String] -> IO()
printSalas4 disciplina inscricao alunos salas [] = return()
printSalas4 disciplina inscricao alunos 0 (x:xs) = return()
printSalas4 disciplina inscricao alunos salas (x:xs) = do
    let disciplinas = unwords (tail (tail (words x)))

    appendFile "exames.txt" ("Classrom" ++ show salas ++ ": " ++ disciplinas ++ "\n")

    encontrarNome disciplinas disciplina inscricao alunos
    printSalas4 disciplina inscricao alunos (salas - 1) xs


tarefa5 :: IO()
tarefa5 = do 
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"

    writeFile "Tarefa5.txt" "" -- limpa a Tarefa.txt 5

    writeFile "Suporte2.txt" ""
    
    writeFile "Suporte3.txt" ""

    putStrLn "Indicate the number of days the exam can take place:"
    dias <- getLine
    putStrLn "Indicate the number of rooms available per day:"
    salas <- getLine

    let n_disciplinas = length (lines conteudoDisciplina)
    let n_dias = read dias :: Int
    let n_salas = read salas :: Int

    if n_disciplinas > n_dias * n_salas
        then putStrLn "Insufficient days to accommodate all exams"
        else distribuir5 (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos) 1 n_salas


copyFile :: [String] -> IO()
copyFile [] = return()
copyFile (x:xs)= do 
    ficheiro <- openFile "Suporte2.txt" AppendMode
    hPutStrLn ficheiro x
    hClose ficheiro
    copyFile xs

distribuir5 :: [String]-> [String]-> [String]->Int -> Int  -> IO() 
distribuir5 disciplina inscricao alunos dias salas  = do

    
    ficheiro <- openFile "Suporte.txt" WriteMode
    hClose ficheiro

    
    ficheiro <- openFile "Tarefa5.txt" AppendMode
    hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
    hClose ficheiro
   -- aqui tenho de fazer a selecao func select
    ficheiro <- openFile "Suporte3.txt" WriteMode
    hClose ficheiro
    
    repeater salas salas -- repete numero de salas ate encontrar a melhor combinacao que guarda no ficheiro 3

    suporte3 <- readFile' "Suporte3.txt"

    ficheiro <- openFile "Suporte.txt" WriteMode
    hClose ficheiro

    printSalas5 disciplina inscricao alunos salas (lines suporte3) --print selecao do suporte 3

    ficheiroSuporte <- readFile "Suporte.txt"
    let tamanhoInicial = length (lines ficheiroSuporte) 
    let tamanhoFinal = length (remDup(lines ficheiroSuporte)) 
    let incompativeis = tamanhoInicial - tamanhoFinal

    ficheiro <- openFile "Tarefa5.txt" AppendMode
    hPutStrLn ficheiro ("Number of incompatibilities:: " ++ show incompativeis) -- print final das incompativilidad
    hClose ficheiro

    suporte2 <- readFile' "Suporte2.txt"
    
    if null (lines suporte2)
        then return()
        else distribuir5 disciplina inscricao alunos (dias+1) salas

     
condTester :: Int -> Int-> [String] -> IO() 
condTester salas _ [] = return()
condTester 0 _ (x:xs) = return()-- tem de remover os valores utilizados do suporte2 e escolher os melhores para o suporte3  
condTester salas salasTotal (x:xs) = do -- numero de salas, ficheiro2
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"

    if salas == salasTotal
        then do 
            suporte3 <- openFile "Suporte3.txt" AppendMode
            hPutStrLn suporte3 x 
            hClose suporte3

            suporte2 <- openFile "Suporte2.txt" WriteMode
            hClose suporte2

            copyFile xs -- remove o x do suporte 2 pois ja foi escolhido
        else do 
            ficheiro <- openFile "Suporte.txt" WriteMode
            hClose ficheiro --limpar o suporte 1 
            suporte3 <- readFile' "Suporte3.txt"
            let size = length (lines suporte3)
            loaderSup2 (lines suporte3) size --adicionar os valores de sup3 no 1 para testar
            -- load x
            let disciplinas = unwords(tail(tail(words x)))
            encontrarNome disciplinas (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)
            --testar
            ficheiroSuporte <- readFile' "Suporte.txt"
            let tamanhoInicial = length (lines ficheiroSuporte) 
            let tamanhoFinal = length (remDup(lines ficheiroSuporte)) 
            let incompativeisx = tamanhoInicial - tamanhoFinal

            if null xs 
                then do 
                    suporte3 <- openFile "Suporte3.txt" AppendMode
                    hPutStrLn suporte3 x 
                    hClose suporte3

                    suporte2 <- openFile "Suporte2.txt" WriteMode
                    hClose suporte2
                    return()
                else do
                    --reload
                    ficheiro <- openFile "Suporte.txt" WriteMode
                    hClose ficheiro --limpar o suporte 1 
                    suporte3 <- readFile' "Suporte3.txt"
                    let size = length (lines suporte3)
                    loaderSup2 (lines suporte3) size --adicionar os valores de sup3 no 1 para testar
                    --load xs
                    let disciplinas = unwords(tail(tail(words (head xs))))
                    encontrarNome disciplinas (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)
                    --testar
                    ficheiroSuporte <- readFile' "Suporte.txt"
                    let tamanhoInicial = length (lines ficheiroSuporte) 
                    let tamanhoFinal = length (remDup(lines ficheiroSuporte)) 
                    let incompativeisxs = tamanhoInicial - tamanhoFinal

                    if incompativeisxs >= incompativeisx
                        then do 
                            suporte3 <- openFile "Suporte3.txt" AppendMode
                            hPutStrLn suporte3 x 
                            hClose suporte3

                            suporte2 <- openFile "Suporte2.txt" WriteMode
                            hClose suporte2

                            copyFile xs -- remove o x do suporte 2 pois ja foi escolhido

                        else do 
                            suporte3 <- openFile "Suporte3.txt" AppendMode
                            hPutStrLn suporte3 (head xs )
                            hClose suporte3

                            suporte2 <- openFile "Suporte2.txt" WriteMode
                            hPutStrLn suporte2 x
                            hClose suporte2
                            
                            copyFile (tail xs) -- remove o x do suporte 2 pois ja foi escolhido

repeater :: Int -> Int-> IO()
repeater 0 salasTotal = return()
repeater salas salasTotal = do 

    suporte2 <- readFile' "Suporte2.txt"
    condTester salas salasTotal (lines suporte2)

    repeater (salas-1) salasTotal


loaderSup2 :: [String] -> Int -> IO()
loaderSup2 [] size = return()
loaderSup2 (x:xs) 0 = return()
loaderSup2 (x:xs) size = do --disciplina e sup3 para comparar size sup3

    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"


    let disciplinas = unwords(tail(tail(words x)))
    encontrarNome disciplinas (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)

    loaderSup2 xs (size-1)


printSalas5 :: [String]-> [String]-> [String]-> Int -> [String] -> IO()
printSalas5 disciplina inscricao alunos salas [] = return()
printSalas5 disciplina inscricao alunos 0 (x:xs) = return()
printSalas5 disciplina inscricao alunos salas (x:xs) = do

    let disciplinas = unwords (tail (tail(words x)))

    ficheiro <- openFile "Tarefa5.txt" AppendMode
    hPutStrLn ficheiro ("Sala "++ show salas ++ ": "++ disciplinas)
    hClose ficheiro

    encontrarNome disciplinas disciplina inscricao alunos
    printSalas5 disciplina inscricao alunos (salas-1) xs
         

tarefa6 :: IO()
tarefa6 = do 
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"


    ficheiro <- openFile "Tarefa6.txt" WriteMode
    hClose ficheiro

    ficheiro2 <- openFile "Suporte2.txt" WriteMode
    hClose ficheiro2 

    formatacao6 (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos) (lines conteudoDisciplina)
    -- texto formatado no ficheiro suporte 2 quantidade nome disciplina

    putStrLn "indicate number of days in which the exam can take place"
    dias <- getLine
    putStrLn "indique o numero de salas disponiveis por dia"
    salas <- getLine
    putStrLn "indicate the capacity of the rooms in the format [clasroom 1, classroom 2,...]"
    lotacao <- getLine
    let lotacaoInt = read lotacao :: [Int]

    let n_dias = read dias :: Int
    let n_salas = read salas :: Int
    let lotacaoReverse = reverse lotacaoInt
    if length (lotacaoReverse) /= n_salas
        then do 
            putStrLn("capacity different from the number of Classrooms")
            return()
        else do 
            conteudoSuporte2 <- readFile' "Suporte2.txt"

            ficheiro <- openFile "Suporte.txt" WriteMode
            hPutStrLn ficheiro (head (lines conteudoSuporte2))  --escrever primeira linha do suporte2 no suporte
            hClose ficheiro
            
            distribuir6 1 n_dias n_salas lotacaoReverse (lines conteudoSuporte2)

            suporte2 <- readFile' "Suporte2.txt"
            if suporte2 /= []
                then putStrLn ("Insufficient to accommodate all exams")
                else return()


--funcoes tarefa6

formatacao6 :: [String] -> [String] -> [String] -> [String] -> IO ()
formatacao6 _ _ _ [] = return ()
formatacao6 disciplina inscricao alunos (x:xs) = do
    let disciplinas = unwords (tail (tail (words x)))

    -- clear file
    ficheiro2 <- openFile "Suporte.txt" WriteMode
    hClose ficheiro2

    suporte1 <- readFile "Suporte.txt"
    let alunosInscritos = length (lines suporte1)

    suporte2 <- openFile "Suporte2.txt" AppendMode
    hPutStrLn suporte2 (show alunosInscritos ++ " " ++ disciplinas)
    hClose suporte2

    formatacao6 disciplina inscricao alunos xs

distribuir6 :: Int -> Int -> Int -> [Int] -> [String] -> IO()
distribuir6 dias diasMax salas lotacao [] = return()
distribuir6 dias diasMax salas lotacao (x:xs) = do
    ficheiro <- openFile "Tarefa6.txt" AppendMode
    hPutStrLn ficheiro ("--- Day "++ show dias ++ "---")
    hClose ficheiro

    printSalas6 salas lotacao (x:xs)

    conteudoSuporte2 <- readFile "Suporte2.txt"
    distribuir6 (dias+1) diasMax salas lotacao (lines conteudoSuporte2)

printSalas6 :: Int -> [Int] -> [String] -> IO()
printSalas6 salas [] [] = return ()
printSalas6 salas [] (x:xs) = return ()
printSalas6 salas (y:ys) [] = return ()
printSalas6 salas (y:ys) (x:xs) = do
    suporte <- readFile "Suporte.txt" -- number of students in the class

    let disciplina = unwords (tail (words x))

    let alunosTurma = head (words suporte)
    let alunosTurmaint = read alunosTurma :: Int
    let alunosRestantes = alunosTurmaint - y

    if alunosTurmaint > y
        then do
            ficheiro <- openFile "Tarefa6.txt" AppendMode
            hPutStrLn ficheiro ("Classroom " ++ show salas ++ " " ++ show y ++ "/" ++ show y ++ ": " ++ disciplina)
            hClose ficheiro
        else do
            ficheiro <- openFile "Tarefa6.txt" AppendMode
            hPutStrLn ficheiro ("Classroom " ++ show salas ++ " " ++ show alunosTurmaint ++ "/" ++ show y ++ ": " ++ disciplina)
            hClose ficheiro

    if alunosRestantes > 0
        then do
            ficheirosuporte <- openFile "Suporte.txt" WriteMode
            hPutStrLn ficheirosuporte (show alunosRestantes ++ " " ++ disciplina)
            hClose ficheirosuporte
            printSalas6 (salas - 1) ys (x : xs)
        else do
            if null xs
                then do
                    ficheiro2 <- openFile "Suporte2.txt" WriteMode -- clear file
                    hClose ficheiro2
                    escreverFicheiro xs
                    printSalas6 (salas - 1) ys xs
                else do
                    ficheirosuporte <- openFile "Suporte.txt" WriteMode
                    hPutStrLn ficheirosuporte (head xs) -- write the first line of Suporte2.txt to Suporte.txt
                    hClose ficheirosuporte
                    ficheiro2 <- openFile "Suporte2.txt" WriteMode -- clear file
                    hClose ficheiro2
                    escreverFicheiro xs
                    printSalas6 (salas - 1) ys xs


escreverFicheiro :: [String] -> IO()
escreverFicheiro [] = return()
escreverFicheiro (x:xs)= do 
            ficheiro2 <- openFile "Suporte2.txt" AppendMode
            hPutStrLn ficheiro2 x
            hClose ficheiro2 
            escreverFicheiro xs