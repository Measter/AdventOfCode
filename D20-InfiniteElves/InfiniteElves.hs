import System.IO
import Data.List

factors :: Int -> [Int]
factors i = a++b
    where
        rtI = (floor.sqrt.fromIntegral.abs) i
        (a,b) = unzip [(j, div i j) | j <- [1..rtI], rem i j == 0]

findHouse :: Int -> Int -> Int
findHouse acc limit =
    if sumOfFactors >= limit
        then acc
        else findHouse (acc+1) limit
    where sumOfFactors = sum $ map (*10) $ factors acc

findHouse2 :: Int -> Int -> Int
findHouse2 acc limit =
    if sumOfFactors >= limit
        then acc
        else findHouse2 (acc+1) limit
    where
        elfFactors = factors acc
        sumOfFactors = foldl (\ac i -> if (div acc i) > 50 then ac else ac + (i*11)) 0 elfFactors

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let parsed = head $ map (\x -> read x :: Int) $ lines contents
        --foundHouse = findHouse 0 parsed
        foundHouse2 = findHouse2 0 parsed

    putStrLn $ show $ foundHouse2

    hClose handle