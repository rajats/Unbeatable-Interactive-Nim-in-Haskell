{-# LANGUAGE MultiWayIf #-}
import System.IO
import Data.Char

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

toBinary :: Int -> [ Int ]
toBinary 0 = [0]     
toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

toDec :: [Int] -> Int
toDec bin = sum (zipWith (*) (reverse (map (\x -> (^) 2 x) [0..length bin -1])) bin)

countOnes  :: [Int] -> Int
countOnes [] = 0
countOnes (x:xs) | x == 1 = 1 + countOnes xs
                 | x == 0 = countOnes xs

--toggles bit at given index
toggle :: [Int] -> [Int] -> Int -> [Int]
toggle [] ys _ = ys
toggle xs [] _ = []
toggle (x:xs) (y:ys) idx | idx == x && y == 1 = 0 : toggle xs ys (idx+1)
                         | idx == x && y == 0 = 1 : toggle xs ys (idx+1) 
                         | otherwise = y : toggle (x:xs) ys (idx+1)

--converts board into binary representation
binBoard :: Board -> [Board]
binBoard board = map toBinary board

--finds maximum length of among binary represenations of numbers in board
maxBinLen :: [Board] -> Int
maxBinLen binBoard = maximum (map length binBoard)

--makes binary representation of numbers in board as same length
sameLenBins :: [Board] -> [Board]
sameLenBins binBoard = map (\x -> replicate (maxBinLen binBoard - length x) 0 ++ x) binBoard

--transpose board in binary representation and count ones for each col, if odd ones in col then for that col xorsum is 1
xorSum :: [Board] -> [Int]
xorSum bins = map (\x -> if 
                 | x `mod` 2 == 0 -> 0 
                 | otherwise -> 1) 
              (map countOnes (transpose bins))

-- zip is used to attach indexes
-- filter (\(x,y) -> x == 1) (zip xsum [0..]) gets tuple of form (colxorsum, idx) where colxorsum is 1
-- map get idx of cols where colxorsum is 1
colsXorSumIs1 :: [Int] -> [Int]
colsXorSumIs1 xsum = map (\(x,y) -> y) (filter (\(x,y) -> x == 1) (zip xsum [0..]))

--Nim from book begins here
next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board] where update r n = if r == row then n-num else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline 
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "ERROR: Invalid digit"
                           getDigit prompt

newline :: IO ()
newline = putChar '\n'

play :: Board -> Int -> IO ()
play board player =
   do newline
      putBoard board
      if finished board then
         do newline
            putStr "Player "
            putStr (show (next player))
            putStrLn " wins!"
      else
         do newline
            putStr "Player " 
            putStrLn (show player)

            if player == 1 then
                do 
                    let sameLenBinsBoard = sameLenBins (binBoard board)
                    let reqdCols = colsXorSumIs1 (xorSum (sameLenBinsBoard))
                    --select rows where there is 1 at column number colsXorSumIs1[0] 
                    let reqdRows =  filter (\(x,y) -> x !! (reqdCols !! 0) == 1)  (zip sameLenBinsBoard [0..])
                    --select one of row, here choosing the first one
                    let row =  snd (reqdRows !! 0)
                    let originialNum = board !! row
                    let numAfterToggle = toDec (toggle reqdCols (sameLenBinsBoard !! row) 0)
                    let num = originialNum - numAfterToggle
                    if valid board (row+1) num then
                       play (move board (row+1) num) (next player)
                    else
                       do newline
                          putStrLn "ERROR: Invalid move"
                          play board player
            else
                do
                    row <- getDigit "Enter a row number: "
                    num <- getDigit "Stars to remove: "
                    if valid board row num then
                       play (move board row num) (next player)
                    else
                       do newline
                          putStrLn "ERROR: Invalid move"
                          play board player
nim :: IO ()
nim = play initial 1