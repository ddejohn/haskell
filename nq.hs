
type Seq = [Char]
type Board = [Seq]
type Diag = [(Int, Int)]


--------------------------------- BOARD SETUP ---------------------------------

setup :: Int -> Board
setup n
    | n < 4 = setup 4
    | otherwise = [replicate n '-' | _ <- [1..n]]

-- I received help from functional programming Discord server on this IO
-- I wrote this simply because I wanted a prettier board to print
-- import Data.List
-- showBoard :: Board -> IO ()
-- showBoard b = putStrLn (intercalate "\n" x)
--     where x = [intercalate "  " [[c] | c <- r] | r <- b]

-- I had also written my own transpose before using import Data.List:
transpose :: Board -> Board
transpose b = [[r!!i | r <- b] | i <- [0..length b - 1]]

rows :: Board -> Int
rows b = length b

cols :: Board -> Int
cols b
    | all (== n) [length r | r <- b] = n
    | otherwise = 0
    where n = rows b

size :: Board -> Int
size b
    | (cols b == rows b) = rows b
    | otherwise = 0

numDiags :: Board -> Int
numDiags b = 2*(size b) - 1

getBoardPos :: Board -> (Int, Int) -> Char
getBoardPos b t = b!!x!!y where (x,y) = t

diags :: Board -> [Diag]
diags b = [[(i+k, j+k) | k <- [0..n-(i+j)]] | [i, j] <- t]
    where n = rows b - 1
          x = [[i,0] | i <- [n, n-1..1]]
          t = x ++ [[0,0]] ++ reverse [reverse y | y <- x]

mainDiag :: Board -> [Seq]
mainDiag b = [map (getBoardPos b) tups | tups <- diags b]

secDiag :: Board -> [Seq]
secDiag b = mainDiag (reverse b)


------------------------------- SOLUTION CHECKS -------------------------------

qSeq :: Seq -> Int
qSeq seq = sum [1 | q <- seq, q == 'Q']

qBoard :: Board -> Int
qBoard b = sum [qSeq r | r <- b]

rVal :: Seq -> Bool
rVal r = qSeq r < 2

cVal :: Seq -> Bool
cVal c = rVal c

allRows :: Board -> Bool
allRows b = and [rVal r | r <- b]

allCols :: Board -> Bool
allCols b = allRows (transpose b)

allDiags :: Board -> Bool
allDiags b = and [qSeq s < 2 | d <- [pri, sec],  s <- d]
    where pri = mainDiag b
          sec = secDiag b

valid :: Board -> Bool
valid b = and [r, c, d]
    where r = allRows b
          c = allCols b
          d = allDiags b

solved :: Board -> Bool
solved b = and [valid b, qBoard b == size b]

-- selectPrimaryDiag :: Board -> Int -> IO ()
-- selectPrimaryDiag b n = do
--     let p = diags b
--     putStrLn [(getBoardPos b) t | t <- p!!n]

-- selectSecondaryDiag :: Board -> Int -> IO ()
-- selectSecondaryDiag b n = selectPrimaryDiag x n
--     where x = reverse b


------------------------------- BOARD GENERATOR -------------------------------