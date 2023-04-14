-- 10001st prime
--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
-- we can see that the 6th prime is 13.
--
-- What is the 10 001st prime number?
prime n | n < 2     = False
        | otherwise = all (\x -> mod n x /= 0)
              (takeWhile (\x -> x^2 <= n) [2..])
primes = filter prime [2..]

main = print (primes !! 10000)
