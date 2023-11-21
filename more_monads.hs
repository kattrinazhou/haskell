import Control.Monad
-- main ::IO()
-- main = forever $ do
--     line <- getLine
--     putStrLn line


exampleMoaybe = Just (Just 3)

flattendedMaybe = join exampleMoaybe

maybeList = [Just 1, Just 2, Just 3]
sequenceResult= sequence maybeList

processItem acc x = if x == 0 then Nothing else Just (acc + x)
example = [1,2,3,4,5]
main ::IO ()
main = do 
    let result = foldM processItem 0 example
    print result