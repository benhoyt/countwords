{-# LANGUAGE OverloadedStrings #-}

module CountWords where

import           Data.Foldable              (traverse_)
import qualified Data.HashMap.Strict        as HMS
import           Data.List                  (sortOn)
import           Data.Ord                   (Down (..))
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO

countwords = sortOn (Down . snd) . HMS.toList . HMS.fromListWith (+) . map (\w -> (T.toLower w, 1))

runCountwords = TIO.getContents >>= traverse_ (\(w,i) -> TIO.putStrLn $ w `T.append` " " `T.append` (T.pack . show $ i)) . countwords . T.words
