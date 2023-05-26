import System.IO ()
lerucs :: IO()
lerucs = do
    ucs <- readFile "ucs.txt"
    correrucs (lines ucs) 

correrucs :: [String] -> IO()
correrucs [] = return ()
correrucs (x:xs)  = do
        putStrLn (unwords(tail(tail(words x))))
        inscricoes <- readFile "inscricoes.txt"
        correrinsc1 (head(words x)) (lines inscricoes)  
        correrucs xs  

correrinsc1 :: String -> [String] -> IO()
correrinsc1 ucs [] = return ()
correrinsc1 ucs (x:xs) = 
    if ucs == last(words x) then
    do
        alunos <- readFile "listaalunos.txt"
        correralunos1 (head(words x)) (lines alunos)
        correrinsc1 ucs xs 
        else
            correrinsc1 ucs xs

correralunos1 :: String -> [String] -> IO ()
correralunos1 al [] = return ()
correralunos1 al (x:xs) = 
    if al == head(words x) then
    do
        putStrLn(unwords(tail(tail(words x))))
    else
        correralunos1 al xs

    






