import System.IO
import Data.List
import Data.Maybe

dimensionsToWrappingPaper :: (Int,Int,Int) -> Int
dimensionsToWrappingPaper (l,w,h) = 2*areaLW + 2*areaLH + 2*areaWH + smallest
    where
        areaLW = l*w
        areaLH = l*h
        areaWH = w*h
        smallest = min areaLH $ min areaLW areaWH

dimensionsToRibbon :: (Int, Int, Int) -> Int
dimensionsToRibbon (l,w,h) = (min lw $ min lh wh) + lwh
    where
        lw = 2*(l+w)
        lh = 2*(l+h)
        wh = 2*(w+h)
        lwh = l*w*h


parseInput :: [String] -> (Int, Int, Int)
parseInput (a:b:c:_) = (read a, read b, read c)

unIntersperse :: (Eq a) => a -> [a] -> [[a]]
unIntersperse _ [] = []
unIntersperse a list =
    case i of
        Nothing -> [list]
        Just _ -> thisSplit : unIntersperse a (drop 1 listTail)
    where
        i = elemIndex a list
        (thisSplit, listTail) = splitAt (fromJust i) list

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let dimensions = (map parseInput . map (unIntersperse 'x') . lines) contents
    (putStrLn . show . sum . map dimensionsToWrappingPaper) dimensions
    (putStrLn . show . sum . map dimensionsToRibbon) dimensions

    hClose handle