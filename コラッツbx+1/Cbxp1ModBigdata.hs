-- module Cbxp1ModBigdata where

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

nを入力してa≡k(mod n)が成り立つ事を確かめるプログラムVer2.3
アルゴリズム：前786 ◆5A/gU5yzeU
コーディング：righ1113 ◆OPKWA8uhcY

> Cbxp1ModBigdata 7    のように実行する
-}

import Data.List (sort, findIndex, (\\), notElem)
import Data.Maybe (fromJust)
import qualified Data.Set as Set (empty, member, insert, toList, fromList, map)
import Data.Char (isDigit)
import System.Environment (getArgs)

-- base = 19___7
-- base = 17
base = 3

-- 速いnub
nubOrd :: (Ord a) => [a] -> [a]
nubOrd l = nubOrd' l Set.empty
  where
    nubOrd' [] _ = []
    nubOrd' (x:xs) s
      | x `Set.member` s = nubOrd' xs s
      | otherwise = x : nubOrd' xs (x `Set.insert` s)

-- 2をかけ続ける
twoTimes :: Int -> Int -> [Int]
twoTimes x n = take n $ iterate (\y -> y*2 `mod` n) x

-- (1)Z/nZにおいて、2を何回かかけることによって移りあう元を同じグループとしてA1,A2,…とグループ分けする。
makeA :: Int -> [[Int]]
makeA n = nubOrd $ map sort [nubOrd $ twoTimes x n | x <- [0..n-1]]
-- makeA n = Set.toList $ Set.map Set.toList $ Set.fromList [Set.fromList $ twoTimes x n | x <- [0..n-1]]

-- 「3の倍数でもnの倍数でもなく、mod nで見た時A'に属さない」
makeB :: [Int] -> Int -> [[Int]]
makeB a' n
  = nubOrd $ map sort
      [nubOrd $ twoTimes x (base*n) | x <- [0..(base*n)-1], x`mod`base /= 0, x`mod`n /= 0, notElem (x`mod`n) a' ]
--  = Set.toList $ Set.map Set.toList $ Set.fromList
--      [Set.fromList $ twoTimes x (base*n) | x <- [0..(base*n)-1], x`mod`base /= 0, x`mod`n /= 0, notElem (x`mod`n) a' ]

-- (4)A'の各元aに対し、3a+1がどのBiに属すかを見る。
findX :: [Int] -> [[Int]] -> Int -> [(Int, Maybe Int)]
findX a' b'' n = nubOrd [(x, findB (base*x+1)) | x <- a']
  where
    findB x = findIndex (elem x) b''
-- オールNothingかどうか
isAllNothing :: [(Int, Maybe Int)] -> Bool
isAllNothing [] = True
isAllNothing ((_, Just _):xs) = False
isAllNothing ((_, Nothing):xs) = isAllNothing xs

-- 表示用＿Nothingを消す
viewFindX :: [(Int, Maybe Int)] -> [Int]
viewFindX findX' = nubOrd $ map fromJust $ [v | (_, v) <- findX', v /= Nothing]

-- 当たりのBiもくっつけて一つのリストにする
search :: [(Int, Maybe Int)] -> [[Int]] -> [Int] -> [Int]
search [] _ acc = nubOrd acc
search ((_, Just x):xs) b' acc = search xs b' acc++(b'!!x)
search (x:xs) b' acc = search xs b' acc

-- 一度も現れなかったBiを見つける
makeFive :: [[Int]] -> [(Int, Maybe Int)] -> Int -> [Int]
makeFive b'' findX' n
  = map fromJust $ [Just z | z <- [0..((length b'') -1)]] \\ [v | (_, v) <- findX', v /= Nothing]
-- 一度も現れなかったBi達をくっつけて一つのリストにする
makeFive2 :: [Int] -> [Int] -> [[Int]] -> [[Int]]
makeFive2 [] [] _ = []
makeFive2 [] acc _ = [acc]
makeFive2 (x:xs) acc b'' = makeFive2 xs (acc ++ (b'' !! x)) b''

-- Z/9rnZにおいて、条件「mod 3nで見た時、(4)で現れていないBiに属する」
makeC :: [Int] -> Int -> Int -> [[Int]]
makeC b'' n r
  = nubOrd $ map sort
    [nubOrd $ twoTimes x (r*base*base*n)
      | x <- [0..(r*base*base*n)-1], x`mod`base /= 0, x`mod`n /= 0, elem (x`mod`(r*base*n)) b'' ]
--  = Set.toList $ Set.map Set.toList $ Set.fromList
--    [Set.fromList $ twoTimes x (r*base*base*n)
--      | x <- [0..(r*base*base*n)-1], x`mod`base /= 0, x`mod`n /= 0, elem (x`mod`(r*base*n)) b'' ]


main :: IO ()
main = do
  let isDigitOnly cs = foldl (\b c -> b && isDigit c ) True cs
  args <- getArgs
  if 1 <= length args
  then do
    let n = (args !! 0)
    if isDigitOnly n
    then
      mainroutine (read n :: Int)
    else
      putStrLn "Argument is not digit"
  else
    putStrLn "Arguments are too short"
  return ()

mainroutine :: Int -> IO ()
mainroutine n = do
  putStrLn ("n="++show(n))
  let dataMakeA = makeA n
  putStrLn ("A : " ++ show(dataMakeA))
  mapM_ (\a -> loop1 a n) dataMakeA
  putStrLn("プログラムは正常終了しました。　"++show(n))

loop1 :: [Int] -> Int -> IO ()
loop1 a' n = do
  putStrLn("")
  putStrLn(show a')
  let dataMakeB = makeB a' n
  putStrLn ("B : " ++ show(map (take 10) dataMakeB))
  let dataFindX = findX a' dataMakeB n
  -- putStrLn ("(4) tuple : " ++ show(dataFindX))
  putStrLn ("得られたB : " ++ show(viewFindX dataFindX))
  let dataMakeFive = makeFive dataMakeB dataFindX n
  putStrLn ("一度も現れなかったBi : " ++ show(dataMakeFive))
  let dataMakeBAtari = search dataFindX dataMakeB []
  mapM_ (\a -> loop2 a dataMakeBAtari n 1 "") (makeFive2 dataMakeFive [] dataMakeB)

loop2 :: [Int] -> [Int] -> Int -> Int -> String -> IO ()
loop2 b'' ba n r dash = do
  putStrLn("")
  let dataMakeC = makeC b'' n r
  putStrLn("C"++dash++" : " ++ show(map (take 10) dataMakeC))
  let dataFindX = findX ba dataMakeC n
  -- putStrLn ("(6)"++dash++" tuple : " ++ show(dataFindX))
  putStrLn ("得られたC"++dash++" : " ++ show(viewFindX dataFindX))
  if isAllNothing dataFindX == True then error ("オールNothingを検出したので強制終了します　"++show(n)) else putStr ""
  let dataMakeFive = makeFive dataMakeC dataFindX n
  putStrLn ("一度も現れなかったCi"++dash++" : " ++ show(dataMakeFive))
  let dataMakeCAtari = search dataFindX dataMakeC []
  -- 繰り返し
  mapM_ (\a -> loop2 a dataMakeCAtari n (r*base) (dash++"'")) (makeFive2 dataMakeFive [] dataMakeC)






