type Seq = [Char]
type Board = [Seq]
type Diag = [(Int, Int)]

setup :: Int -> Board
setup n
    | n < 4 = setup 4
    | otherwise = [replicate n '-' | _ <- [1..n]]

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

qSeq :: Seq -> Int
qSeq seq = sum [1 | q <- seq, q == 'Q']

qBoard :: Board -> Int
qBoard b = sum [qSeq r | r <- b]

rVal :: Seq -> Bool
rVal r = qSeq r < 2

cVal :: Seq -> Bool
cVal c = rVal c

allRows :: Board -> Bool
allRows b = all (== True) [rVal r | r <- b]

allCols :: Board -> Bool
allCols b = allRows (transpose b)

diags :: Board -> [Diag]
diags b = [[(i+k, j+k) | k <- [0..n-(i+j)]] | [i, j] <- t]
    where n = rows b - 1
          x = [[0,i] | i <- [1..n]]
          t = [[0,0]] ++ x ++ [reverse y | y <- x]

getBoardPos :: Board -> (Int, Int) -> Char
getBoardPos b t = b!!x!!y where (x,y) = t

primaryDiag :: Board -> [Seq]
primaryDiag b = [map (getBoardPos b) tups | tups <- diags b]

secondaryDiag :: Board -> [Seq]
secondaryDiag b = primaryDiag (transpose b)

-- main = do
--     let b = setup 4
--     print(b)
--     print(rows b)
--     print(cols b)
--     print(size b)
--     print(qSeq "--Q--Q-Q")
--     print(qBoard ["Q---", "--Q-", "-Q-Q", "QQQQ"])
    