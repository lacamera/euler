-- Special Pythagorean triplet
--
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a2 + b2 = c2
-- For example, 32 + 42 = 9 + 16 = 25 = 52.
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.
pt a b = a < b && b < c && (a^2+b^2 == c^2)
  where c = 1000 - a - b

ans = [a*b*(1000-a-b)|
  a <- [1..1000], b <- [a+1..1000], pt a b]

main = print $ (head $ ans)
