import System.IO
import Data.List
import Data.Maybe

data Actor = Actor Int Int Int deriving (Show)
data ItemList = ItemList (Maybe Weapon) (Maybe Armour) (Maybe Ring) (Maybe Ring) deriving (Show)
data Weapon = Dagger | Shortsword | Warhammer | Longsword | Greataxe deriving (Show, Enum)
data Armour = Leather | Chainmail | Splintmail | Bandedmail | Platemail deriving (Show, Enum)
data Ring = Attack1 | Attack2 | Attack3 | Defence1 | Defence2 | Defence3 deriving (Show, Enum, Eq)

getWeaponCost :: (Maybe Weapon) -> Int
getWeaponCost w =
    case w of
        Just Dagger -> 8
        Just Shortsword -> 10
        Just Warhammer -> 25
        Just Longsword -> 40
        Just Greataxe -> 74
        otherwise -> 0

getWeaponDamage :: (Maybe Weapon) -> Int
getWeaponDamage w =
    case w of
        Just Dagger -> 4
        Just Shortsword -> 5
        Just Warhammer -> 6
        Just Longsword -> 7
        Just Greataxe -> 8
        otherwise -> 0

getArmourCost :: (Maybe Armour) -> Int
getArmourCost w =
    case w of
        Just Leather -> 13
        Just Chainmail -> 31
        Just Splintmail -> 53
        Just Bandedmail -> 75
        Just Platemail -> 102
        otherwise -> 0

getArmourArmour :: (Maybe Armour) -> Int
getArmourArmour w =
    case w of
        Just Leather -> 1
        Just Chainmail -> 2
        Just Splintmail -> 3
        Just Bandedmail -> 4
        Just Platemail -> 5
        otherwise -> 0

getRingCost :: (Maybe Ring) -> Int
getRingCost w =
    case w of
        Just Attack1 -> 25
        Just Attack2 -> 50
        Just Attack3 -> 100
        Just Defence1 -> 20
        Just Defence2 -> 40
        Just Defence3 -> 80
        otherwise -> 0

getRingDamage :: (Maybe Ring) -> Int
getRingDamage w =
    case w of
        Just Attack1 -> 1
        Just Attack2 -> 2
        Just Attack3 -> 3
        otherwise -> 0

getRingArmour :: (Maybe Ring) -> Int
getRingArmour w =
    case w of
        Just Defence1 -> 1
        Just Defence2 -> 2
        Just Defence3 -> 3
        otherwise -> 0

parseInput :: String -> Actor
parseInput str = Actor hp dam arm
    where
        parts = words str
        hp = read $ parts!!2
        dam = read $ parts!!4
        arm = read $ parts!!6



getItemLists :: [Maybe Weapon] -> [Maybe Armour] -> [Maybe Ring] -> [ItemList]
getItemLists weaps arms rings = [ItemList w a r1 r2 | w <- weaps, a <- arms, r1 <- rings, r2 <- rings,
                                    r1 /= r2 || (isNothing r1 && isNothing r2)]

getItemListCost :: ItemList -> Int
getItemListCost (ItemList w a r1 r2) = (getWeaponCost w) + (getArmourCost a) + (getRingCost r1) + (getRingCost r2)

getItemListDamage :: ItemList -> Int
getItemListDamage (ItemList w _ r1 r2) = (getWeaponDamage w) + (getRingDamage r1) + (getRingDamage r2)

getItemListArmour :: ItemList -> Int
getItemListArmour (ItemList _ a r1 r2) = (getArmourArmour a) + (getRingArmour r1) + (getRingArmour r2)


comboCheck :: Int -> Actor -> ItemList -> Bool
comboCheck pH (Actor bossHealth bossDamage bossArmour) items =
    playerRounds <= bossRounds
    where
        playerDamage = getItemListDamage items
        playerArmour = getItemListArmour items
        bossRounds = div pH (max 1 (bossDamage - playerArmour))
        playerRounds = div bossHealth (max 1 (playerDamage - bossArmour))

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let playerHealth = 100
        boss = parseInput contents
        weapons = map (Just) [Dagger .. Greataxe]
        armour = Nothing:(map (Just) [Leather .. Platemail])
        rings = Nothing:(map (Just) [Attack1 .. Defence3])

        allCombos = getItemLists weapons armour rings
        goodCombos = filter (comboCheck playerHealth boss) allCombos
        badCombos = filter (not . comboCheck playerHealth boss) allCombos
        cheapest = minimumBy (\c1 c2 -> compare (getItemListCost c1) (getItemListCost c2)) goodCombos
        dearest = maximumBy (\c1 c2 -> compare (getItemListCost c1) (getItemListCost c2)) badCombos

    putStrLn $ show cheapest
    putStrLn $ show $ getItemListCost cheapest
    putStrLn $ show dearest
    putStrLn $ show $ getItemListCost dearest

    hClose handle