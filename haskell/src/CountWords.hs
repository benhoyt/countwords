{-# LANGUAGE OverloadedStrings #-}

module CountWords where

import           Data.Foldable              (traverse_)
import qualified Data.HashMap.Strict        as HMS
import           Data.List                  (sortOn)
import           Data.Ord                   (Down (..))
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO

countwords :: [T.Text] -> [(T.Text, Int)]
countwords = go HMS.empty 
    where
    go mmap [] = sortOn (Down . snd) . HMS.toList $ mmap
    go mmap (x:xs) = let w = T.append (T.toLower x) " " in case HMS.lookup w mmap of
        Nothing -> go (HMS.insert w 1 mmap) xs
        Just v  -> go (HMS.insertWith (\_ _ -> v + 1) w v mmap) xs

runCountwords :: IO ()
runCountwords = TIO.getContents >>= traverse_ (\(w,i) -> TIO.putStrLn $ w `T.append` (T.pack . show $ i)) . countwords . T.words