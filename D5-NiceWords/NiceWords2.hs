import System.IO
import Data.List

hasSplitPair :: String -> Bool
hasSplitPair [] = False
hasSplitPair (_:_:[]) = False
hasSplitPair str@(a:_:b:_) = a==b || hasSplitPair (drop 1 str)

getLetterPairs :: String -> [String]
getLetterPairs [] = []
getLetterPairs str@(_:_:_) = (take 2 str) : getLetterPairs (drop 1 str)
getLetterPairs (_:_) = []

-- Credit goes to /u/ratmatix on Reddit for finding the edge-case of 4 consecutive
-- letters that I missed on this function, and providing the fix.
checkOverLap :: [(Int,String)] -> Bool
checkOverLap [] = False
checkOverLap ((i,_):(j,_):[]) = j-i == 1
checkOverLap (_:_:_) = False

hasSplitDoubleLetterPair :: String -> Bool
hasSplitDoubleLetterPair str = (length filteredPairs > 0) && not anyOverLaps
    where
        pairs = zip [0..] $ getLetterPairs str
        groupedPairs = groupBy (\(i,a) (j,b) -> a == b) $ sortBy (\(i,a) (j,b) -> compare a b) pairs
        filteredPairs = filter (\x -> length x >= 2) groupedPairs
        anyOverLaps = any (==True) $ map checkOverLap filteredPairs


isNiceWord :: String -> Bool
isNiceWord str = hasSplitPair str && hasSplitDoubleLetterPair str

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    (putStrLn . show . length . filter isNiceWord . lines) contents

    hClose handle