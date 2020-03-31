type Seq = [Char]
type Board = [Seq]


setup :: Int -> Board
transpose :: Board -> Board
rows :: Board -> Int
cols :: Board -> Int
size :: Board -> Int

queensSeq :: Seq -> Int
queensBoard :: Board -> Int

col_val :: Seq -> Bool
row_val :: Seq -> Bool

all_rows_val :: Board -> Bool
all_cols_val :: Board -> Bool

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
    where n = length (head b)

size b
    | (cols b == rows b) = rows b
    | otherwise = 0

queensSeq seq = sum [1 | q <- seq, q == 'Q']
queensBoard b = sum [queensSeq r | r <- b]

row_val r = queensSeq r < 2
col_val c = row_val c

all_rows_val b = all (== True) [row_val r | r <- b]
all_cols_val b = all_rows_val (transpose b)

-- main = do
--     let b = setup 4
--     print(b)
--     print(rows b)
--     print(cols b)
--     print(size b)
--     print(queensSeq "--Q--Q-Q")
--     print(queensBoard ["Q---", "--Q-", "-Q-Q", "QQQQ"])
    