-- 10001st prime
--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
-- we can see that the 6th prime is 13.
--
-- What is the 10 001st prime number?

-- Trial division for simplicity.
primes = s [2..] where
    s (p:xs) = p:s [x| x <- xs, (mod x p) > 0]

main = print $ primes !! 10000
