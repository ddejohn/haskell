f :: Integer -> Integer
add :: Integer -> Integer -> Integer
mul :: Integer -> Integer -> Integer
summate :: [Integer] -> Integer
divisors :: Integer -> [Integer]
primes :: Integer -> [Integer]
is_prime :: Integer -> Bool

f x = x * x
add x y = x + y
mul x y = x * y
summate [] = 0
summate lst = (head lst) + summate (tail lst)
divisors x = [n | n <- [1..x], (mod x n) == 0]
is_prime x = length (divisors x) == 2

-- list of primes up to and including x
primes x = [p | p <- [2..x], is_prime p]

main = do
    -- print (f 3)
    -- print (add 2 3)
    -- print (mul 3 8)
    -- print (summate [1..5])
    -- print (is_prime 4)
    -- print (divisors 114)
    print (primes 113)