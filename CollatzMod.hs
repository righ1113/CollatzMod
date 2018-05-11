-- module CollatzMod where

{-
定義
自然数aに対し、集合T(a)を
T(a)={b∈N|aとbはコラッツ操作によって同じ数に到達する}
と定める。
T(a)の形の集合を木と呼ぶ。

コラッツ予想が真であることは、自然数全体が１つの木をなすことと同値である。
で、次のように予想した。

予想
Tを木とし、n,kを自然数とする。
このとき、あるa∈Tが存在してa≡k(mod n)が成り立つ。

nを入力してa≡k(mod n)が成り立つ事を確かめるプログラムVer1.2
アルゴリズム：786 ◆5A/gU5yzeU
コーディング：righ1113 ◆OPKWA8uhcY
-}

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

-- loop1
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

-- loop2
makeD :: Int -> Int -> (Int -> [[Int]]) -> Int -> Int -> [[Int]]
makeD x1 x2 funcMakeC r p
  = nub $ map sort [nub $ twoTimes y (r*27*p) | y <- [0..(r*27*p)-1], elem (y `mod` (r*9*p)) ((funcMakeC x1) !! x2) ]

-- (8)
makeDAfter :: Int -> (Int -> [[Int]]) -> [(Int, Maybe Int)] -> Int -> [Int]
makeDAfter x1 funcMakeC dataFindY p
  = concat [(funcMakeC x1) !! y |
      y <- intersect [0..((length $ (funcMakeC x1)) -1)] [fromJust v | (k, v) <- dataFindY, v /= Nothing]]
findD :: Int -> Int -> Int -> (Int -> [[Int]]) -> Int -> Maybe Int
findD x y1 y2 funcMakeC p = findIndex (elem x) (makeD y1 y2 funcMakeC 1 p)
findZ :: Int -> Int -> (Int -> [[Int]]) -> [(Int, Maybe Int)] -> Int -> [(Int, Maybe Int)]
findZ x1 x2 funcMakeC dataFindY p
  = nub [(fromJust $ (findC y x1 p), findD (3*y+1 `mod` p) x1 x2 funcMakeC p)
          | y <- makeDAfter x1 funcMakeC dataFindY p]
  where
    findC x y p = findIndex (elem x) (funcMakeC y)

-- ()一度も現れなかったDiがあれば
makeEight :: Int -> Int -> (Int -> [[Int]]) -> [(Int, Maybe Int)] -> Int -> [Int]
makeEight y1 y2 funcMakeC dataFindY p
  = map fromJust
    $ [Just z | z <- [0..((length $ makeD y1 y2 funcMakeC 1 p) -1)]]
      \\ [v | (_, v) <- findZ y1 y2 funcMakeC dataFindY p, v /= Nothing]


main :: IO ()
main = do
  putStrLn ("素数pを入力してください")
  pStr <- getLine
  let p = read pStr :: Int
  putStrLn ("Z/pZ : " ++ show([0..p-1]))
  putStrLn ("A : " ++ show(makeA p))
  putStrLn ("Z/3pZ : " ++ show([0..(3*p)-1]))
  putStrLn ("B : " ++ show(makeB p))
  putStrLn ("(3) tuple : " ++ show(findX p))
  let dataMakeFour = makeFour p
  putStrLn ("(4) A' No. : " ++ show(dataMakeFour))
  mapM_ (\q1 -> loop1 q1 p) dataMakeFour
  putStrLn("プログラムは正常終了しました。")

loop1 :: Int -> Int -> IO ()
loop1 q1 p = do
  putStrLn("")
  putStrLn ("C : " ++ show(makeC q1 p))
  putStrLn ("(6) tuple : " ++ show(findY q1 p))
  let dataMakeSeven = makeSeven q1 p
  putStrLn ("一度も現れなかったCi : " ++ show(dataMakeSeven))
  mapM_ (\q2 -> loop2 q1 q2 (\q3 -> makeC q3 p) (findY q1 p) 1 p "") dataMakeSeven

loop2 :: Int -> Int -> (Int -> [[Int]]) -> [(Int, Maybe Int)] -> Int -> Int -> String -> IO ()
loop2 q1 q2 funcMakeC dataFindY r p dash = do
  putStrLn("")
  let funcMakeD = (\q2 -> makeD q1 q2 funcMakeC r p)
  putStrLn ("D" ++ dash ++ " : " ++ show(funcMakeD q2))
  let dataFindZ = findZ q1 q2 funcMakeC dataFindY p
  putStrLn ("(8)" ++ dash ++ " tuple : " ++ show(dataFindZ))
  let dataMakeEight = makeEight q1 q2 funcMakeC dataFindY p
  putStrLn ("一度も現れなかったDi" ++ dash ++ " : " ++ show(dataMakeEight))
  -- 繰り返し
  mapM_ (\q3 -> loop2 q2 q3 funcMakeD dataFindZ (r*3) p (dash++"'")) dataMakeEight



