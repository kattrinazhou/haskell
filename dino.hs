data Dinosaur = Dinosaur Bool Bool Int deriving Eq
pterodactyl = Dinosaur True True 2
stegosaurus = Dinosaur False True 4
diplodocus = Dinosaur False False 4


-- formspikey :: Bool -> String -> [Char]
formspikey _ [] = []
formspikey bool (x:xs) =
    if bool then x ++ '^' : formspikey bool xs
    else x ++ '-' : formspikey bool xs

instance Show Dinosaur where
    show (Dinosaur iswinged isspikey numlegs) =
        if iswinged
            then ">" ++ representlegs ++ "<"
            else representlegs
        where
            representlegs = init (formspikey isspikey (replicate numlegs 'b'))