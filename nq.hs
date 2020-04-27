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

allMainDiagIndices :: Board -> [Diag]
allMainDiagIndices b = [[(i+k, j+k) | k <- [0..n-(i+j)]] | [i, j] <- t]
    where n = rows b - 1
          x = [[i,0] | i <- [n, n-1..1]]
          t = x ++ [[0,0]] ++ reverse [reverse y | y <- x]

allSecDiagIndices :: Board -> [Diag]
allSecDiagIndices b = do
    let n = length b
    let x = [[(j, i-j) | j <- [i,i-1..0]] | i <- [0..n-1]]
    let y = [[(j, n-1-j+k) | j <- [n-1,n-2..k]] | k <- [1..n-1]]
    x ++ y

mainDiagIndices :: Board -> Int -> Diag
mainDiagIndices b n = (allMainDiagIndices b)!!n

secDiagIndices :: Board -> Int -> Diag
secDiagIndices b n = (allSecDiagIndices b)!!n

mainDiag :: Board -> [Seq]
mainDiag b = [map (getBoardPos b) tups | tups <- allMainDiagIndices b]

secDiag :: Board -> [Seq]
secDiag b = mainDiag (reverse b)


------------------------------- SOLUTION CHECKS -------------------------------

queensSeq :: Seq -> Int
queensSeq seq = sum [1 | q <- seq, q == 'Q']

queensBoard :: Board -> Int
queensBoard b = sum [queensSeq r | r <- b]

seqValid :: Seq -> Bool
seqValid r = queensSeq r < 2

rowsValid :: Board -> Bool
rowsValid b = and [seqValid r | r <- b]

colsValid :: Board -> Bool
colsValid b = rowsValid (transpose b)

diagsValid :: Board -> Bool
diagsValid b = and [queensSeq s < 2 | d <- [pri, sec],  s <- d]
    where pri = mainDiag b
          sec = secDiag b

valid :: Board -> Bool
valid b = and [r, c, d]
    where r = rowsValid b
          c = colsValid b
          d = diagsValid b

solved :: Board -> Bool
solved b = and [valid b, queensBoard b == size b]


------------------------------- BOARD GENERATOR -------------------------------

setQueenAt :: Board -> Int -> [Board]
setQueenAt b i = do
    let z = replicate ((size b) - 1) '-'
    let p = nub (permutations ("Q" ++ z))
    [ [ (b!!k) | k <- [0..(i-1)] ] ++ [r] ++ [ (b!!k) | k <- [(i+1)..((rows b) - 1)] ] | r <- p ]

nextRow :: Board -> Int
nextRow b = head [ i | i <- [0 .. (size b) - 1], queensSeq (b!!i) == 0 ]

solve :: Board -> [Board]
solve b
    | solved b = [b]
    | otherwise = concat [ solve newB | newB <- setQueenAt b i, valid newB ]
        where i = nextRow b

main = do
    let b = setup 6
    let soln = [soln | soln <- solve b]
    mapM_ (showBoard) soln
