type Seq = [Char]
type Board = [Seq]


setup :: Int -> Board
transpose :: Board -> Board
rows :: Board -> Int
cols :: Board -> Int
size :: Board -> Int

qSeq :: Seq -> Int
qBoard :: Board -> Int

cVal :: Seq -> Bool
rVal :: Seq -> Bool

allRows :: Board -> Bool
allCols :: Board -> Bool

-- primaryDiag :: Board -> Int -> [(Int, Int)]
-- secondaryDiag :: Board -> Int -> [(Int, Int)]

setup n
    | n < 4 = setup 4
    | otherwise = [replicate n '-' | _ <- [1..n]]

transpose b = [[r!!i | r <- b] | i <- [0..length b - 1]]

rows b = length b
cols b
    | all (== n) [length r | r <- b] = n
    | otherwise = 0
    where n = rows b
size b
    | (cols b == rows b) = rows b
    | otherwise = 0

qSeq seq = sum [1 | q <- seq, q == 'Q']
qBoard b = sum [qSeq r | r <- b]

rVal r = qSeq r < 2
cVal c = rVal c

allRows b = all (== True) [rVal r | r <- b]
allCols b = allRows (transpose b)

-- main = do
--     let b = setup 4
--     print(b)
--     print(rows b)
--     print(cols b)
--     print(size b)
--     print(qSeq "--Q--Q-Q")
--     print(qBoard ["Q---", "--Q-", "-Q-Q", "QQQQ"])
    