import qualified Data.Set as Set
import System.IO

data House = House Int Int deriving (Show, Ord, Eq)

charToCoordChange :: Char -> (Int, Int) -> (Int, Int)
charToCoordChange c (x, y)
    | c == '>' = (x+1,y)
    | c == '<' = (x-1,y)
    | c == 'v' = (x,y+1)
    | c == '^' = (x,y-1)
    | otherwise = (x,y)

countHouses :: String -> House -> Set.Set House -> Int
countHouses [] _ s = length s
countHouses (s:st) (House x y) set =
    countHouses st newFatMan newSet
    where
        (newX, newY) = charToCoordChange s (x,y)
        newFatMan = House newX newY
        newSet = Set.insert newFatMan set

countRoboHouses :: String -> House -> House -> Set.Set House -> Int
countRoboHouses [] _ _ s = length s
countRoboHouses (s:r:st) (House sX sY) (House rX rY) set =
    countRoboHouses st newSanta newRobo newSet'
    where
        (newSantaX, newSantaY) = charToCoordChange s (sX,sY)
        (newRoboX, newRoboY) = charToCoordChange r (rX,rY)
        newSanta = House newSantaX newSantaY
        newRobo = House newRoboX newRoboY
        newSet = Set.insert newSanta set
        newSet' = Set.insert newRobo newSet

countRoboHouses instr@(s:st) h _ set = countHouses instr h set

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    (putStrLn.show) $ countHouses contents (House 0 0) (Set.fromList [House 0 0])
    (putStrLn.show) $ countRoboHouses contents (House 0 0) (House 0 0) (Set.fromList [House 0 0])
    hClose handle