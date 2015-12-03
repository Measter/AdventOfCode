import Data.List
import System.IO

parseInstruction :: Int -> Char -> Int
parseInstruction acc x = acc + i
    where
        i = case x of
            '(' -> 1
            ')' -> -1
            otherwise -> 0

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    ( putStrLn . show . (foldl parseInstruction 0) ) contents
    ( putStrLn . show . length . (takeWhile (>=0)) . (map (foldl parseInstruction 0)) . inits) contents

    hClose handle