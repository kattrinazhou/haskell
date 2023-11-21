-- {-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

import GHC.Builtin.Types (nothingDataCon)
import Data.List(elemIndex)
import Data.Maybe (catMaybes, isJust)
import GHC.Data.Maybe (isJust)



data Player = Red | Yellow
    deriving (Eq)
data Board = MkBoard { board :: [[Player]], numRows :: Int, numCols :: Int }

type RowID = Int
type ColumnID = Int
type RowCount = Int
type ColCount = Int


type Row = [Maybe Player]
type PaddedColumn = [Maybe Player]
type Column = [Player]
type Diagonal = [Maybe Player]

{- Toggles the current player -}
togglePlayer :: Player -> Player
togglePlayer Red = Yellow
togglePlayer Yellow = Red

{- Board accessors / manipulation function -}

{- Q1(a): emptyBoard -}
emptyBoard :: RowCount -> ColCount -> Board
emptyBoard rs cs = MkBoard { board = replicate rs [], numRows = rs, numCols = cs }

getCounter :: Board -> RowID -> ColumnID -> Maybe Player
getCounter b r c
  | r >= numRows b || r < 0 || c >= numCols b || c < 0 = error "oops, out  of bounds"
  | otherwise = isPlayer b r c



isPlayer :: Board -> RowID -> ColumnID -> Maybe Player
isPlayer b r c
  | c < 0 || c >= length (board b) || r < 0 || r >= length (board b)= Nothing
  | otherwise =
    case reverse (board b !! c) of
            columnlist ->
                if r < length columnlist
                    then
                        case columnlist !! r of
                            Red -> Just Red
                            Yellow -> Just Yellow
                            _ -> Nothing
                else Nothing

{- Q1(c): getRow
 - Retrieves the list of counters on the given row -}
getRow :: Board -> RowID -> [Maybe Player]
getRow b r
    |r < 0 || r >= numRows b = error "co-ordinates out of bounds!"
    |otherwise = [getCounter b r c | c <-[0..(numCols b -1 )]]

{- Q1(d): getColumn
 - Retrieves the list of counters in the given column, from top-to-bottom -}
getColumn :: Board -> ColumnID -> PaddedColumn
getColumn b c
    |c < 0 || c >= numCols b = error "co-ordinates out of bounds!"
    |otherwise = reverse [getCounter b r c | r <-[0..(numCols b - 1 )]]

instance Show Player where
    show :: Player -> String
    show p = case p of
        Red -> "Red"
        Yellow -> "Yellow"

instance Show Board where
    show :: Board -> String
    show b = unlines [ map toString (getRow b r)| r <- reverse [0..numRows b -1]]
        where
            toString :: Maybe Player -> Char
            toString element =
                case element of
                    Just Yellow -> 'Y'
                    Just Red -> 'R'
                    Nothing -> '0'

{- Drops a counter into the given column. If the move is legal, returns an updated
 - board. Otherwise returns Nothing. -}
dropCounter :: Board -> ColumnID -> Player -> Maybe Board
dropCounter b c p
    | c >= numCols b || c < 0 = Nothing --check out of bound
    | not (ifNothing columnlist) = Nothing --check if full
    | otherwise = Just (MkBoard (updateboard (board b)) (numRows b) (numCols b))
    where
        replacePlayer [] = []
        replacePlayer (x:xs)
            | x == Nothing = Just p : xs
            | otherwise = x : replacePlayer xs
        columnlist = reverse (getColumn b c)

        ifNothing = foldr (\x acc -> acc || x == Nothing) False
        convertlist = catMaybes (replacePlayer columnlist)

        updateboard :: [[Player]] -> [[Player]]
        updateboard list = [if i == c then convertlist else x | (i, x) <- zip [0..] list]

{- Q4: Diagonals -}
-- consider the difference between row and col
getTLBRDiagonals :: Board -> [[Maybe Player]]
getTLBRDiagonals b = reverse (map (reverse . getcoodinate) [0..(numRows b + numCols b - 2)])
    where
        getcoodinate n = [getCounter b r (n - r) | r <- [(rowindexmin n)..(rowindexmax n)]]
        rowindexmin n = max 0 (n - numCols b + 1)
        rowindexmax n = min n (numRows b - 1)

getBLTRDiagonals :: Board -> [Diagonal]
getBLTRDiagonals b =  map getCoordinate [- (numRows b - 1)..(numCols b - 1)]
    where
        getCoordinate n = [getCounter b r (n + r) | r <- [ (rowindexmin n) .. (rowindexmax n)]]
        rowindexmin n = max 0 (-n)
        rowindexmax n = min (numRows b - 1) (numCols b - 1 - n)

{- Q5: Win checking -}
{- Checks if the given list has a subsequence of length 4, returning Just Player
 - if so, Nothing otherwise -}
hasFourInRow :: [Maybe Player] -> Maybe Player
hasFourInRow [] = Nothing
hasFourInRow (x:xs) =
    case x of
      Just p -> if checkIfFour p (x:xs) 0
                        then Just p
                    else hasFourInRow xs
      Nothing -> hasFourInRow xs
    where
            checkIfFour _ [] count = count >= 4
            checkIfFour p (x:xs) count =
                if x == Just p
                    then checkIfFour p xs (count + 1)
                else count >= 4


{- Checks all rows, columns, and diagonals for any subsequences of length 4 -}
checkWin :: Board -> Maybe Player
checkWin b
    | isJust (checklist checkRow) =  winnerRow
    | isJust (checklist checkColumn) =  winnerColumn
    | isJust (checklist checkTLBRD) =  winnerTLBRD
    | isJust (checklist checkBRTLD) =  winnerBRTLD
    | otherwise = Nothing
    where
        checkRow = [hasFourInRow (getRow b r) | r <- [0 .. (numRows b - 1)]]
        checkColumn = [hasFourInRow (getColumn b c) | c <- [0 .. (numCols b - 1)]]
        checkTLBRD = map hasFourInRow (getTLBRDiagonals b)
        checkBRTLD = map hasFourInRow (getBLTRDiagonals b)

        winnerRow = checklist checkRow
        winnerColumn = checklist checkColumn
        winnerTLBRD = checklist checkTLBRD
        winnerBRTLD = checklist checkBRTLD

        checklist :: [Maybe Player] -> Maybe Player
        checklist [] = Nothing
        checklist (x:xs) =
            case x of
                Just _ -> x
                Nothing -> checklist xs






-- {- Instance -}
-- instance Show Board wherez
--     show :: Board -> String
--     show b = unlines (map (unwords . map show)(board b))

record :: Board
record = MkBoard {board= [[Red],[Red,Yellow],[],[],[]], numRows = 5, numCols = 5}
record2 = MkBoard {board= [[],[],[],[],[]], numRows = 5, numCols = 5}
record3 = MkBoard {board= [[Red],[Red,Yellow],[Red,Yellow,Red],[Red,Yellow,Yellow,Yellow],[]], numRows = 5, numCols = 5}
record4 = MkBoard {board= [[Yellow,Red],[Yellow,Yellow,Yellow],[Yellow,Yellow,Yellow,Yellow,Yellow],[Red],[]], numRows = 5, numCols = 5}
printrec = unlines ["00000","00000","00000","0R000","RY000"]
