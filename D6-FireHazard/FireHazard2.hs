import Data.List
import Data.Maybe
import Data.Array
import System.IO

data Operation = TurnOn | TurnOff | Toggle deriving (Show)
data Point = Point Int Int deriving (Show)
data Instruction = Instruction Operation Point Point deriving (Show)

parsePoint :: String -> Point
parsePoint str = Point x y
    where
        commaIndex = elemIndex ',' str
        (xStr, yStr) = splitAt (fromJust commaIndex) str
        x = read $ xStr
        y = read $ drop 1 yStr

parseInput :: String -> Instruction
parseInput str =
    Instruction op topLeft topRight
    where
        theWords = delete "turn" $ delete "through" $ words str

        op = case head theWords of
            "off" -> TurnOff
            "on" -> TurnOn
            "toggle" -> Toggle
            otherwise -> error ("Unknown Instruction " ++ (head theWords))
        topLeft = parsePoint $ theWords !! 1
        topRight = parsePoint $ theWords !! 2

updateRowSegment :: Operation -> [Int] -> [Int]
updateRowSegment op seg =
    map (\x -> max 0 (x+inc) ) seg
    where
        inc = case op of
            TurnOn -> 1
            TurnOff -> -1
            Toggle -> 2

applyInstruction :: Instruction -> Array Int (Array Int Int) -> Array Int (Array Int Int)
applyInstruction inst@(Instruction op (Point tlX tlY) (Point brX brY)) arr =
    applyToggleInner tlY brY arr
    where
        applyToggleInner :: Int -> Int -> Array Int (Array Int Int) -> Array Int (Array Int Int)
        applyToggleInner cur end arr =
            if cur == (end+1) then arr
            else
                applyToggleInner (cur+1) end newArr
                where
                    curRow = arr!cur
                    changedElems = zip [tlX..] $ updateRowSegment op $ take (brX-(tlX-1)) $ drop tlX $ elems curRow
                    newRow = curRow//changedElems
                    newArr = arr//[(cur,newRow)]


outputArrayRow :: Handle ->  [Int] -> IO ()
outputArrayRow handle row = do
    mapM_ (hPutStr handle) $ map (\x -> " " ++ show x ++ " ") row
    hPutStrLn handle ""

outputArray :: Array Int (Array Int Int) -> IO ()
outputArray arr = do
    handle <- openFile "test.txt" WriteMode
    mapM_ (outputArrayRow handle) $ map (elems) $ elems arr
    hClose handle

arrayRows = 999
arrayColumns = 999

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let emptyRow = array (0,arrayColumns) [(x,0) | x <- [0..arrayColumns]]
        field = array (0,arrayRows) [(y, emptyRow) | y <- [0..arrayRows]]
        instructions = map parseInput $ lines contents
        finalArray = foldl (\arr ins -> applyInstruction ins arr) field instructions

    outputArray finalArray
    putStrLn $ show $ sum $ concat $ map elems $ elems finalArray

    hClose handle