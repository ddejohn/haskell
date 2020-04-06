import Data.List

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
-- was told to use intercalate, which required import Data.List
-- I did this before I realized that all of your source code was on github
-- I didn't know I wasn't supposed to do this from scratch

-- import Data.List
showBoard :: Board -> IO ()
showBoard b = putStrLn (intercalate "\n" x)
    where x = [intercalate " " [[c] | c <- r] | r <- b] ++ ["\n"]

-- I had also written my own transpose before using import Data.List:
-- transpose :: Board -> Board
-- transpose b = [[r!!i | r <- b] | i <- [0..length b - 1]]

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


------------------------------- BOARD GENERATOR -------------------------------

setQueenAt :: Board -> Int -> [Board]
setQueenAt b i = do
    let z = replicate ((size b) - 1) '-'
    let p = nub (permutations ("Q" ++ z))
    [ [ (b!!k) | k <- [0..(i-1)] ] ++ [r] ++ [ (b!!k) | k <- [(i+1)..((rows b) - 1)] ] | r <- p ]

nextRow :: Board -> Int
nextRow b = head [ i | i <- [0 .. (size b) - 1], qSeq (b!!i) == 0 ]

solve :: Board -> [Board]
solve b
    | solved b = [b]
    | otherwise = concat [ solve newB | newB <- setQueenAt b i, valid newB ]
        where i = nextRow b

main = do
    let b = setup 6
    let soln = [soln | soln <- solve b]
    mapM_ (showBoard) soln
