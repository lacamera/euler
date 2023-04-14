-- Largest prime factor
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143?
pfs n = pfs' n 2
  where
    pfs' n f
      | f*f > n      = [n]
      | mod n f == 0 = f:pfs' (div n f) f
      | otherwise    = pfs' n (f+1)

main = print (maximum $ pfs 600851475143)
