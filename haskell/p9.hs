-- Special Pythagorean triplet
--
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a2 + b2 = c2
-- For example, 32 + 42 = 9 + 16 = 25 = 52.
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.
import Euler (pyt)

ans = [a*b*(1000-a-b)|
  a <- [1..1000], b <- [a+1..1000], pyt a b]

main = print . (head $ ans)
