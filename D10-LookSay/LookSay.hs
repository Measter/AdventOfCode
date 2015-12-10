import Data.List
import System.IO

lookSay :: Int -> String -> String
lookSay 0 str = str
lookSay i str = lookSay (i-1) $ lookSay' str
    where lookSay' = concat . map (\x -> (show $ length x) ++ [(head x)]) . group

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    putStrLn $ show $ length $ lookSay 40 $ head $ lines contents
    putStrLn $ show $ length $ lookSay 50 $ head $ lines contents

    hClose handle