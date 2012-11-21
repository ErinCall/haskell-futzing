import Data.Char
import Data.List

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstEquallingForty :: Maybe Int
firstEquallingForty = firstEqualling 40
--firstEquallingForty = find equalsForty [1..]
--  where equalsForty x = 40 == digitSum x

firstEqualling :: Int -> Maybe Int
firstEqualling n = find (\x -> digitSum x == n) [1..]
