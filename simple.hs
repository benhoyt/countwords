import Data.List
import Data.Ord(Down(..))
import qualified Data.Map as M

countwords :: [String] -> [(String, Int)] 
countwords [] = []
countwords ws =
    let seed = M.empty
        go mmap [] = sortOn (Down . snd) . M.toList $ mmap
        go mmap (x:xs) = case M.lookup x mmap of
            Nothing -> go (M.insert x 1 mmap) xs
            Just v -> go (M.insertWith (\_ _ -> v + 1) x v mmap) xs
    in  go seed ws

main = getLine >>= print . show . countwords . words