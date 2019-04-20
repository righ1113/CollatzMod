module Collatz5 where

import Data.List (unfoldr)
import Control.Monad (forM_)

colSeq :: Int -> [Int]
colSeq = unfoldr f where
    f 0 = Nothing
    f 1 = Just (1, 0)
    f x
        | even x    = Just (x, x `div` 2)
        | otherwise = Just (x, (x*3+1) `div` 2)

toBit :: Int -> Int
toBit x = if even x then 0 else 1

-- 2進数から10進数
bin2int :: [Int] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

-- 10進数からb進数
expand :: Integral a => a -> a -> [a]
expand b = unfoldr f
  where f 0 = Nothing
        f x = let (q, r) = divMod x b
              in Just (r, q)

-- check
colCheck :: Int -> IO ()
colCheck cnt =
  forM_ [1,3..cnt] $ \x ->
--    if x == bin2int (map toBit $ take (length $ expand 2 x) $ colSeq x) then print x else putStr ""

    print $ bin2int $ map toBit $ take (length $ expand 2 x) $ colSeq x
--    print $ map toBit $ take 100 $ colSeq x

--    print $ bin2int $ map toBit $ take 100 $ colSeq x



