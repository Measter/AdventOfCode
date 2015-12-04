import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Numeric (showHex)
import System.IO
import Data.List (isPrefixOf)

prettyPrint :: String -> BS.ByteString -> String
prettyPrint acc bs
    | BS.length bs == 0 = acc
    | otherwise = cur ++ prettyPrint acc (BS.tail bs)
                    where
                        x = BS.head bs
                        digit = showHex x ""
                        cur = if length digit == 1 then '0':digit else digit


doHash :: String -> BS.ByteString
doHash = (MD5.hash . BSC.pack)

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let hashes = map (\x -> (x, (prettyPrint "" . doHash) $ contents++show x)) [0..]
        fiveFilteredHashes = filter (\(i,h) -> isPrefixOf "00000" h) hashes
        sixFilteredHashes = filter (\(i,h) -> isPrefixOf "000000" h) hashes

    putStrLn $ show $ take 1 fiveFilteredHashes
    putStrLn $ show $ take 1 sixFilteredHashes

    hClose handle