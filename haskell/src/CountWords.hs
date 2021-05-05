{-# LANGUAGE OverloadedStrings #-}

module CountWords where

import           Data.Char       (toLower)
import           Data.Foldable   (Foldable (foldl'))
import           Data.List       (sortBy)
import qualified Data.Map.Strict as M
import           Data.Ord        (Down (..), comparing)
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           GHC.Unicode     (isSpace)

countwords :: Ord a => [a] -> M.Map a Integer
countwords = foldl' (\m k -> M.insertWith (+) k 1 m) M.empty
{-# SPECIALIZE countwords :: [T.Text] -> M.Map T.Text Integer #-}

runCountWords :: IO ()
runCountWords =
    T.getContents >>= T.putStr
        . T.unlines
        . map (\(w, i) -> w `T.append` " " `T.append` (T.pack . show $ i))
        . sortBy (comparing (Down . snd))
        . M.toList
        . countwords
        . filter (not . T.null)
        . T.split isSpace
        . T.map toLower