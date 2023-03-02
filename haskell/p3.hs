-- Largest prime factor
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143?
pf n = pf' n 2
  where
    pf' n f
      | f*f > n      = [n] -- return early
      | mod n f == 0 = f:pf' (div n f) f
      | otherwise    = pf' n (f+1)

main = print $ maximum (pf 600851475143)
