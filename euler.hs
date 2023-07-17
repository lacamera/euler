import Data.List (tails)

fib a b = a : fib b (a + b)
prime n | n < 2     = False
        | otherwise = all (\x -> mod n x /= 0)
              (takeWhile (\x -> x^2 <= n) [2..])
primes = filter prime [2..]
pfs n = pfs' n 2
  where
    pfs' n f
      | f*f > n      = [n]
      | mod n f == 0 = f:pfs' (div n f) f
      | otherwise    = pfs' n (f+1)

-- Multiples of 3 or 5
-- Find the sum of all the multiples of 3 or 5 below 1000.
p1 = print (sum [x| x <- [1..999], (mod x 3) == 0 || (mod x 5) == 0])

-- Even Fibonacci numbers
-- By considering the terms in the Fibonacci sequence whose values
-- do not exceed four million, find the sum of the even-valued terms.
p2 = print (sum . filter even $ takeWhile (<= 4000000) (fib 1 2))

-- Largest prime factor
-- What is the largest prime factor of the number 600851475143?
p3 = print (maximum $ pfs 600851475143)

-- Largest palindrome product
-- Find the largest palindrome made from the product of
-- two 3-digit numbers.
p4 = print (maximum [a*b| a <- [100..999], b <- [100..999],
  (reverse $ show (a*b)) == show (a*b)])

-- Smallest multiple
-- What is the smallest positive number that is
-- evenly divisible by all of the numbers from 1 to 20?
p5 = print (foldl1 lcm [1..20])

-- Sum square difference
-- Find the difference between the sum of the squares
-- of the first one hundred natural numbers and the square of the sum.
p6 = print (sum [1..100]^2 - sum [x^2| x <- [1..100]])

-- 10001st prime
-- What is the 10 001st prime number?
p7 = print (primes !! 10000)

-- Largest product in a series
-- Find the thirteen adjacent digits in the
-- 1000-digitnumber that have the greatest product.
-- What is the value of this product?
str =
  "731671765313306249192251196744265747423553\
  \4919493496983520312774506326239578318016984801869478\
  \8518438586156078911294949545950173795833195285320880\
  \5511125406987471585238630507156932909632952274430435\
  \5766896648950445244523161731856403098711121722383113\
  \6222989342338030813533627661428280644448664523874930\
  \3589072962904915604407723907138105158593079608667017\
  \2427121883998797908792274921901699720888093776657273\
  \3300105336788122023542180975125454059475224352584907\
  \7116705560136048395864467063244157221553975369781797\
  \7846174064955149290862569321978468622482839722413756\
  \5705605749026140797296865241453510047482166370484403\
  \1998900088952434506585412275886668811642717147992444\
  \2928230863465674813919123162824586178664583591245665\
  \2947654568284891288314260769004224219022671055626321\
  \1111093705442175069416589604080719840385096245544436\
  \2981230987879927244284909188845801561660979191338754\
  \9920052406368991256071760605886116467109405077541002\
  \2569831552000559357297257163626956188267042825248360\
  \0823257530420752963450"
x = map (read . (: "")) str
-- zips [1,2,3,..] to [[1,2],[2,3],..] for 13 digits
gs = zipWith const (take 13 <$> tails x) (drop (13-1) x)
p8 = print (maximum $ map product gs)

-- Special Pythagorean triplet
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.
spt a b = a < b && b < c && (a^2+b^2 == c^2)
  where c = 1000 - a - b
ts = [a*b*(1000-a-b)| a <- [1..1000], b <- [a+1..1000], spt a b]
p9 = print $ (head $ ts)

-- Summation of primes
-- Find the sum of all the primes below two million.
p10 = print (sum $ takeWhile (<2000000) primes)
