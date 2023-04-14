-- Summation of primes
--
-- The sum of the primes below 10 is 2+3+5+7=17.
--
-- Find the sum of all the primes below two million.
prime n | n < 2     = False
        | otherwise = all (\x -> mod n x /= 0)
              (takeWhile (\x -> x^2 <= n) [2..])
primes = filter prime [2..]

main = print (sum $ takeWhile (<2000000) primes)
