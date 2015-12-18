import System.IO
import Data.List

--function taken from https://wiki.haskell.org/99_questions/Solutions/26
combinations :: [a] -> Int -> [[a]]
combinations _ 0 = [[]]
combinations xs n = [y:ys | y:xs' <- tails xs, ys <- combinations xs' (n-1)]

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let parsed = map (\x -> read x :: Int) $ lines contents
        allCombs = concat $ map (combinations parsed) [1..length parsed]
        combsSumTo = filter ((==)150.sum) allCombs
        sortedCombs = sortBy (\x y -> compare (length x) (length y)) combsSumTo
        groupedByLength = groupBy (\x y -> (length x) == (length y)) sortedCombs

    putStrLn $ show $ length combsSumTo
    putStrLn $ show $ length $ head groupedByLength

    hClose handle