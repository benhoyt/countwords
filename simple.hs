module Main where

import Text.Printf
import Data.List
import Data.Char
import qualified Data.Map as M

count :: [String] -> M.Map String Int -> M.Map String Int
count xs m = foldl (\ m x -> M.insertWith (+) x 1 m) m xs

countWords :: String -> M.Map String Int
countWords line = count (words line) M.empty

printCount :: [(String, Int)] -> IO ()
printCount [] = putStr ""
printCount ((a,b):xs) = do
    putStrLn (printf "%s %d" a b)
    printCount xs

main :: IO ()
main = do
    contents <- getContents
    let counted = countWords (map toLower contents)
        sorted = sortBy (\ (_,a) (_,b) -> compare b a) $ M.toList counted
    printCount sorted
