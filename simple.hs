module Main where

import Data.Char (toLower)
import Data.List (sortBy)
import qualified Data.Map as M

main :: IO ()
main = (putStrLn . prettify . sortCount . wc) =<< getContents
  where
    sortCount = sortBy (\(_, a) (_, b) -> compare b a) . M.toList
    prettify = unlines . fmap (\(w, c) -> w ++ " " ++ show c)

wc :: String -> M.Map String Int
wc = foldr insertWord M.empty . words
  where insertWord word = M.insertWith (+) (toLower <$> word) 1
