module Main where
import qualified Data.Set as Set
import Data.List
import System.IO

data Reindeer = Reindeer String Int Int Int deriving (Show)
data Points = Points String Int deriving (Show)

instance Eq Points where
    (Points idA _) == (Points idB _) = idA == idB
    (Points idA _) /= (Points idB _) = idA /= idB

instance Ord Points where
    compare (Points idA _) (Points idB _) = compare idA idB

parseInput :: String -> Reindeer
parseInput str =
    Reindeer name speed flightTime restTime
    where
        wordList = words str
        name = head wordList
        speed = read $ wordList!!3
        flightTime = read $ wordList!!6
        restTime = read $ wordList!!13

calcTravelDistance :: Int -> Reindeer -> (String, Int)
calcTravelDistance time (Reindeer name speed travelTime restTime) =
    (name, distancedTravelled)
    where
        distancePerFullCycle = speed * travelTime
        cycleTime = travelTime + restTime
        cycleCount = div time cycleTime
        cycleDistance = cycleCount * distancePerFullCycle

        partialCycleTravelTime = min (time - (cycleCount*cycleTime)) travelTime
        partialDistance = partialCycleTravelTime * speed

        distancedTravelled = if time >= travelTime
            then cycleDistance + partialDistance
            else partialDistance

calcPoints :: Int -> [Reindeer] -> Set.Set Points -> Set.Set Points
calcPoints 0 _ acc = acc
calcPoints time deer acc = calcPoints (time-1) deer newAcc
    where
        leaders = head $ groupBy (\(_,x) (_,y) -> x==y) $ sortBy (\(_,x) (_,y) -> compare y x) $ map (calcTravelDistance time) deer
        pointedLeaders = Set.intersection acc $ Set.fromList $ map (\(n,_) -> Points n 0) leaders
        updatedPointLeaders = Set.map (\(Points n p) -> Points n (p+1)) pointedLeaders
        newAcc = Set.union updatedPointLeaders acc

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let parsed = map parseInput $ lines contents
        fastest = maximumBy (\(_,x) (_,y) -> compare x y) $ map (calcTravelDistance 2503) parsed
        pointSet = Set.fromList $ map (\(Reindeer name _ _ _) -> Points name 0) parsed
        mostPoints = maximumBy (\(Points _ x) (Points _ y) -> compare x y) $ Set.toAscList $ calcPoints 2503 parsed pointSet

    putStrLn $ show fastest
    putStrLn $ show mostPoints

    hClose handle