add :: Integer -> Integer -> Integer
add x y = x + y


mul :: Integer -> Integer -> Integer
mul x y = x * y


sum' :: [Integer] -> Integer
sum' [] = 0
sum' lst =  add (head lst) (sum' (tail lst))


divisors :: Integer -> [Integer]
divisors x = [n | n <- [1..x], (mod x n) == 0]


is_prime :: Integer -> Bool
is_prime x = length (divisors x) == 2


gcd' :: Integer -> Integer -> Integer
gcd' a b
    | b == 0 = a
    | a < b = gcd' b a
    | otherwise = gcd' b (mod a b)


coprime :: Integer -> Integer -> Bool
coprime a b = gcd' a b == 1


primes :: Integer -> [Integer]
primes n = [p | p <- filter is_prime [2..n]]


totient :: Integer -> Int
totient m = length [r | r <- [1..m-1], coprime m r]


primeFactors :: Integer -> [Integer]
primeFactors n = [m | m <- divisors n, is_prime m]


main = do
    -- print (add 2 3)
    -- print (mul 3 8)
    -- print (sum' [1..9])
    -- print (is_prime 4)
    -- print (divisors 114)
    print (primes 113)
    -- print (gcd' 12 4)
    -- print (coprime 13 14)
    print (totient 123)
    -- print (primeFactors 128)