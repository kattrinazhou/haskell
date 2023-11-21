import Data.Char
echoCaps :: IO ()
echoCaps = do
    x <- getLine
    putStrLn $ "Here is your capitalises version of sentense : " ++ map toUpper x



-- echoFile :: FilePath -> IO ()
echoFile path = do 
    content <- readFile path
    putStrLn content


-- calculator :: IO ()
-- calculator = do
    
