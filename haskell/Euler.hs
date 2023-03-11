module Euler (pyt, pfs, prime, primes)
  where

pyt :: Int -> Int -> Bool
pt a b = a < b && b < c && (a^2+b^2 == c^2)
  where c = 1000 - a - b

pfs :: Int -> [Int]
pfs n = pfs' n 2
  where
    pfact' n f
      | f*f > n      = [n]
      | mod n f == 0 = f:pfs' (div n f) f
      | otherwise    = pfs' n (f+1)

prime :: Int -> Bool
prime n | n < 2     = False
        | otherwise = all (\x -> mod n x /= 0)
              (takeWhile (\x -> x^2 <= n) [2..])

primes :: [Int]
primes = filter prime [2..]
