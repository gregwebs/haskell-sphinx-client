module Text.Search.Sphinx.Put where

import Data.Binary.Put
import Data.ByteString.Lazy hiding (pack, length, map, groupBy)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as BS
import qualified Text.Search.Sphinx.Types as T

num = putWord32be . toEnum
num64 = putWord64be . toEnum
enum :: Enum a => a -> Put
enum = num . fromEnum
numList ls = do num (length ls)
                mapM_ num ls
nums cfg   = mapM_ (\x -> num $ x cfg)
num64s cfg = mapM_ (\x -> num64 $ x cfg)

stringIntList :: [(String, Int)] -> Put
stringIntList xs = num (length xs) >> mapM_ strInt xs
 where strInt (s,i) = str s >> num i

str :: String -> Put
str s = do let bs = pack s
           num (fromEnum $ BS.length bs)
           putLazyByteString bs

cmd :: T.SearchdCommand -> Put
cmd = putWord16be . toEnum . T.searchdCommand

verCmd :: T.VerCommand -> Put
verCmd = putWord16be . toEnum . T.verCommand
