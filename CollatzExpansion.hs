-- module CollatzExpansion where

{-
・nビット以下の01列とそれに対応するコラッツ展開をもつ2^n以下の自然数を列挙する
・コーディング：righ1113 ◆OPKWA8uhcY
> CollatzExpansion 3    のように実行する
-}

import Data.Char (isDigit)
import System.Environment (getArgs)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

makeXRev :: Int -> [Bit] -> [Bit]
makeXRev n bits = reverse $ take n (bits ++ repeat 0)

-- 0,1,0,1,2,3,0,1,2,3,4,5,6,7...を作る
powList :: Int -> [[Bit]]
powList n = concatMap (\a -> map (makeXRev a . int2bin) [0..(2^a-1)]) [1..n]
--2新数を文字列にする
bit2Char :: Bit -> Char
bit2Char 0 = '0'
bit2Char 1 = '1'
bit2Char _ = '?'
powList2 :: [[Bit]] -> [String]
powList2 = map (map bit2Char)

-- コラッツ
col :: Int -> Int
col n | odd n     = (3*n+1) `div` 2
      | otherwise =       n `div` 2
collatz :: Int -> Int -> [Int]
collatz step n = take step $ iterate col n
-- 偶奇
oddEven :: Int -> Bit
oddEven n | odd n     = 1
          | otherwise = 0

collatzAll :: Int -> [(Int, [Bit])]
collatzAll n = zip [1..2^n] $ map (map oddEven . collatz n) [1..2^n]

answer :: Int -> [[Int]]
answer n =
  map
    (map fst .
       (\ a ->
          filter (\ b -> a == take (length a) (snd b)) $ collatzAll n))
    (powList n)


main :: IO ()
main = do
  let isDigitOnly = foldl (\b c -> b && isDigit c) True
  args <- getArgs
  if 1 <= length args
  then do
    let n = head args
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
  putStrLn ("n="++show n)
  mapM_ print $ zip (powList2 $ powList n) (answer n)
  -- putStrLn (show(collatzAll 3))
  putStrLn("プログラムは正常終了しました。　"++show n)
