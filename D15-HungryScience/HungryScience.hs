import System.IO
import Data.List

data Ingredient = Ingredient Int Int Int Int Int deriving (Show)
data Type = Capacity | Durability | Flavour | Texture | Calories

parseInput :: String -> Ingredient
parseInput str =
    Ingredient cap dur flav tex cal
    where
        theWords = words str
        cap = read $ delete ',' $ theWords!!2
        dur = read $ delete ',' $ theWords!!4
        flav = read $ delete ',' $ theWords!!6
        tex = read $ delete ',' $ theWords!!8
        cal = read $ theWords!!10

getTypeValue :: Type -> (Ingredient, Int) -> Int
getTypeValue prop ((Ingredient cap dur flav tex cal), count) =
    case prop of
        Capacity -> cap * count
        Durability -> dur * count
        Flavour -> flav * count
        Texture -> tex * count
        Calories -> cal * count

getCookieValue :: [(Ingredient, Int)] -> (Int, Int)
getCookieValue ingredAmount =
    (capacity * durability * flavour * texture, calories)
    where
        capacity = max 0 $ sum $ map (getTypeValue Capacity) ingredAmount
        durability = max 0 $ sum $ map (getTypeValue Durability) ingredAmount
        flavour = max 0 $ sum $ map (getTypeValue Flavour) ingredAmount
        texture = max 0 $ sum $ map (getTypeValue Texture) ingredAmount
        calories = max 0 $ sum $ map (getTypeValue Calories) ingredAmount

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let parsed = map parseInput $ lines contents
        range = [0..100]
        cookieAmounts = [zip parsed [a,b,c,d] | a <- range, b <- range, c <- range, d <- range, a + b + c + d == 100]
        computedCookies = map getCookieValue cookieAmounts

    putStrLn $ show $ maximumBy (\(x,_) (x2,_) -> compare x x2) computedCookies
    putStrLn $ show $ maximumBy (\(x,_) (x2,_) -> compare x x2) $ filter (\(_,y) -> y == 500) computedCookies

    hClose handle