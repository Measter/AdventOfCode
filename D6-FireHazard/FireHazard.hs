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

assembleOnOffRowUpdate :: Instruction -> [(Int,Bool)]
assembleOnOffRowUpdate (Instruction op (Point tlX _) (Point brX _)) =
    [(x, val) | x <- [tlX .. brX]]
    where
        val = case op of
            TurnOn -> True
            TurnOff -> False
            Toggle -> error "Invalid instruction."

assembleToggleRowUpdate :: [(Bool)] -> [(Bool)]
assembleToggleRowUpdate row = map not row

applyInstruction :: Instruction -> Array Int (Array Int Bool) -> Array Int (Array Int Bool)
applyInstruction inst@(Instruction op (Point tlX tlY) (Point brX brY)) arr =
    case op of
        Toggle -> applyToggleInner tlY brY arr
        _ -> applyOnOffInner tlY brY arr
    where
        onOffRow = assembleOnOffRowUpdate inst
        applyOnOffInner cur end arr =
            if cur == (end+1) then arr
            else
                applyOnOffInner (cur+1) end newArr
                where
                    newRow = (arr!cur)//onOffRow
                    newArr = arr//[(cur,newRow)]
        applyToggleInner :: Int -> Int -> Array Int (Array Int Bool) -> Array Int (Array Int Bool)
        applyToggleInner cur end arr =
            if cur == (end+1) then arr
            else
                applyToggleInner (cur+1) end newArr
                where
                    curRow = arr!cur
                    changedElems = zip [tlX..] $ assembleToggleRowUpdate $ take (brX-(tlX-1)) $ drop tlX $ elems curRow
                    newRow = curRow//changedElems
                    newArr = arr//[(cur,newRow)]


outputArrayRow :: Handle ->  [Bool] -> IO ()
outputArrayRow handle row = do
    mapM_ (hPutStr handle) $ map (\x -> if x then "1" else "0") row
    hPutStrLn handle ""

outputArray :: Array Int (Array Int Bool) -> IO ()
outputArray arr = do
    handle <- openFile "test.txt" WriteMode
    mapM_ (outputArrayRow handle) $ map (elems) $ elems arr
    hClose handle

arrayRows = 999
arrayColumns = 999

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let emptyRow = array (0,arrayColumns) [(x,False) | x <- [0..arrayColumns]]
        field = array (0,arrayRows) [(y, emptyRow) | y <- [0..arrayRows]]
        instructions = map parseInput $ lines contents
        finalArray = foldl (\arr ins -> applyInstruction ins arr) field instructions

    putStrLn $ show $ sum $ map (\x -> if x then 1 else 0) $ concat $ map elems $ elems finalArray

    hClose handle