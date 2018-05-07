module CollatzMod where

import Data.List (nub, sort, findIndex, (\\), intersect)
import Data.Maybe (fromJust)

twoTimes :: Int -> Int -> [Int]
twoTimes x p = take p $ iterate (\y -> y*2 `mod` p) x

makeA :: Int -> [[Int]]
makeA p = nub $ map sort [nub $ twoTimes x p | x <- [0..p-1]]
makeB :: Int -> [[Int]]
makeB p = nub $ map sort [nub $ twoTimes x (3*p) | x <- [0..(3*p)-1], x `mod` 3 /= 0, x `mod` p /= 0]

findA :: Int -> Int -> Int
findA x p = fromJust $ findIndex (elem x) (makeA p)
findB :: Int -> Int -> Maybe Int
findB x p = findIndex (elem x) (makeB p)
findX :: Int -> [(Int, Maybe Int)]
findX p = nub [(findA x p, findB (3*x+1 `mod` p) p) | x <- [0..p-1]]

-- (4)A1,A2,…のうち、全てのBjとの組が得られていないもの を調査
makeFour' :: Int -> Int -> Bool
makeFour' x p = [Just y | y <- [0..((length $ makeB p) -1)]] /= sort [v | (k, v) <- findX p, v /= Nothing, k==x]
makeFour :: Int -> [Int]
makeFour p = filter (\x -> makeFour' x p) [0..((length $ makeA p) -1)]

-- 組(A',Bj)が得られていないようなBj を見つける
makeCBefore :: Int -> Int -> [Int]
makeCBefore x p = [0..((length $ makeB p) -1)] \\ [fromJust v | (k, v) <- findX p, v /= Nothing, k==x]
-- Bjの元
makeCBefore2 :: Int -> Int -> [Int]
makeCBefore2 x p = concat [(makeB p) !! y | y <- makeCBefore x p]
makeC :: Int -> Int -> [[Int]]
makeC x p = nub $ map sort [nub $ twoTimes y (9*p) | y <- [0..(9*p)-1], elem (y `mod` (3*p)) (makeCBefore2 x p)]

makeCAfter :: Int -> Int -> [Int]
makeCAfter x p
  = concat [(makeB p) !! y | y <- intersect [0..((length $ makeB p) -1)] [fromJust v | (k, v) <- findX p, v /= Nothing, k==x]]
findC :: Int -> Int -> Int -> Maybe Int
findC x y p = findIndex (elem x) (makeC y p)
findY :: Int -> Int -> [(Int, Maybe Int)]
findY x p = nub [(fromJust $ findB y p, findC (3*y+1 `mod` p) x p) | y <- makeCAfter x p]

-- (7)一度も現れなかったCiがあれば
makeSeven :: Int -> Int -> [Int]
makeSeven y p
  = map fromJust $ [Just z | z <- [0..((length $ makeC y p) -1)]] \\ [v | (_, v) <- findY y p, v /= Nothing]

makeD :: Int -> Int -> Int -> [[Int]]
makeD x1 x2 p = nub $ map sort [nub $ twoTimes y (27*p) | y <- [0..(27*p)-1], elem (y `mod` (9*p)) ((makeC x1 p) !! x2) ]

-- (8)
makeDAfter :: Int -> Int -> [Int]
makeDAfter x1 p -- (x1, x2) = (0, 2)
  = concat [(makeC x1 p) !! y |
      y <- intersect [0..((length $ makeC x1 p) -1)] [fromJust v | (k, v) <- findY x1 p, v /= Nothing, k==x1]]
findD :: Int -> Int -> Int -> Int -> Maybe Int
findD x y1 y2 p = findIndex (elem x) (makeD y1 y2 p)
findZ :: Int -> Int -> Int -> [(Int, Maybe Int)]
findZ x1 x2 p = nub [(fromJust $ findC y x1 p, findD (3*y+1 `mod` p) x1 x2 p) | y <- makeDAfter x1 p]

-- ()一度も現れなかったDiがあれば
makeEight :: Int -> Int -> Int -> [Int]
makeEight y1 y2 p
  = map fromJust $ [Just z | z <- [0..((length $ makeD y1 y2 p) -1)]] \\ [v | (_, v) <- findZ y1 y2 p, v /= Nothing]

main = do
  putStrLn ("素数pを入力してください")
  pStr <- getLine
  let p = read pStr :: Int
  putStrLn ("Z/pZ : " ++ show([0..p-1]))
  putStrLn ("A : " ++ show(makeA p))
  putStrLn ("Z/3pZ : " ++ show([0..(3*p)-1]))
  putStrLn ("B : " ++ show(makeB p))
  putStrLn ("(3) tuple : " ++ show(findX p))
  putStrLn ("(4) A' No. : " ++ show(makeFour p))
  let q1 = 0 -- A' No.
  putStrLn ("C : " ++ show(makeC q1 p))
  putStrLn ("(6) tuple : " ++ show(findY q1 p))
  putStrLn ("一度も現れなかったCi : " ++ show(makeSeven q1 p))
  let q2 = 2
  putStrLn ("D : " ++ show(makeD q1 q2 p))
  putStrLn ("(8) tuple : " ++ show(findZ q1 q2 p))
  putStrLn ("一度も現れなかったDi : " ++ show(makeEight q1 q2 p))




