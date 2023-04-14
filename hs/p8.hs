-- Largest product in a series
--
-- The four adjacent digits in the 1000-digit number that
-- have the greatest product are 9×9×8×9 = 5832.
--
-- Find the thirteen adjacent digits in the
-- 1000-digitnumber that have the greatest product.
-- What is the value of this product?
import Data.List (tails)

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

-- zips [1,2,3,..] to [[1,2],[2,3],..] for 13 adjacent digits
gs = zipWith const (take 13 <$> tails x) (drop (13-1) x)

main = print (maximum $ map product gs)