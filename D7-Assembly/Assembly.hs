import System.IO
import qualified Data.Set as Set
import Data.Word
import Data.List
import Data.Maybe (fromJust)
import Data.Char (isDigit)
import Data.Bits

data Instruction = Instruction String (Either [String] Word16) deriving (Show)

instance Eq Instruction where
    (Instruction idA _) == (Instruction idB _) = idA == idB
    (Instruction idA _) /= (Instruction idB _) = idA /= idB

instance Ord Instruction where
    compare (Instruction idA _) (Instruction idB _) = compare idA idB

readInstruction :: String -> Instruction
readInstruction str =
    Instruction label value
    where
        theWords = words str
        label = last theWords
        arrowIndex = elemIndex "->" theWords
        leftSide = take (fromJust arrowIndex) theWords
        value = if length leftSide == 1 && all isDigit (head leftSide)
            then Right $ read (head leftSide)
            else Left leftSide

getInstructionByLabel :: String -> Set.Set Instruction -> Instruction
getInstructionByLabel label set =
    let index = Set.findIndex (Instruction label $ Right 0) set -- God-awful hack.
    in Set.elemAt index set

applyInstruction :: String -> Word16 -> Word16 -> Word16
applyInstruction "AND" a b = a .&. b
applyInstruction "OR" a b = a .|. b
applyInstruction "LSHIFT" a b = shift a (fromIntegral b)
applyInstruction "RSHIFT" a b = shift a (-(fromIntegral b))

parseValueList :: [String] -> String -> Set.Set Instruction -> (Word16, Set.Set Instruction)
parseValueList (val:[]) _ set = getValue (getInstructionByLabel val set) set
parseValueList ("NOT":lab:[]) label set =
    (finalValue, set'')
    where
        (finalValue, set'') = if all isDigit lab
                                then let parsed = complement (read lab) in (parsed, Set.insert (Instruction label (Right parsed)) set)
                                else let (val, set') = (getValue (getInstructionByLabel lab set) set); val' = complement val
                                        in (val', Set.insert (Instruction label (Right val')) set')

parseValueList (a:instr:b:[]) label set =
    (finalVal, Set.insert (Instruction label (Right finalVal)) set'')
    where
        (aVal, set') = if all isDigit a
                            then (read a, set)
                            else parseValueList [a] label set
        (bVal, set'') = if all isDigit b
                            then (read b, set')
                            else parseValueList [b] label set'
        finalVal = applyInstruction instr aVal bVal

getValue :: Instruction -> Set.Set Instruction -> (Word16, Set.Set Instruction)
getValue inst@(Instruction label val) set =
    (finalValue, set'')
    where
        (finalValue, set'') = case val of
            Right r -> (r, set)
            Left list -> parseValueList list label set

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let instrSet = Set.fromList $ map readInstruction $ lines contents
        aVal = fst $ getValue (getInstructionByLabel "a" instrSet) instrSet
        part2Set = Set.insert (Instruction "b" (Right aVal)) instrSet
        part2AVal = fst $ getValue (getInstructionByLabel "a" part2Set) part2Set

    putStrLn $ show aVal
    putStrLn $ show part2AVal

    hClose handle