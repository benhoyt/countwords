import Data.Char
import Data.List
import Data.Ord(Down(..))
import qualified Data.Map as M

countwords :: [String] -> [(String, Int)] 
countwords [] = []
countwords ws =
    let seed = M.empty
        go mmap [] = sortOn (Down . snd) . M.toList $ mmap
        go mmap (x:xs) = 
            let w = map toLower x
            in  case M.lookup w mmap of
                Nothing -> go (M.insert w 1 mmap) xs
                Just v -> go (M.insertWith (\_ _ -> v + 1) w v mmap) xs
    in  go seed ws

main = getLine >>= print . show . countwords . words