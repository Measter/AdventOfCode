import System.IO
import Data.List

vowelCount = (sum . map checkVowel)
    where checkVowel ch = if elem ch "eaiou" then 1 else 0

hasDoubleLetters :: String -> Bool
hasDoubleLetters xs = any (\x -> length x >= 2) $ group xs

isBannedPair :: String -> Bool
isBannedPair = flip elem ["ab", "cd", "pq", "xy"]

hasBannedPair :: String -> Bool
hasBannedPair [] = False
hasBannedPair (_:[]) = False
hasBannedPair str = isBannedPair pair || hasBannedPair rest
    where
        pair = take 2 str
        rest = drop 1 str

isNiceWord :: String -> Bool
isNiceWord str = (vowelCount str >= 3) && (hasDoubleLetters str) && (not $ hasBannedPair str)

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    (putStrLn . show . length . filter isNiceWord . lines) contents

    hClose handle