module Project where

ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y

factorial :: Integer -> Integer
factorial n = product [1..n]

(&&&) :: Bool -> Bool -> Bool
True  &&& b = b
_ &&& _ = False

-- elem :: Int -> [Int] -> Bool
-- n `elem` [] = False
-- n `elem` (x:xs) = x == n || n `elem` xs

run = undefined