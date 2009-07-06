module Text.Search.Sphinx.Put where

import Data.Int (Int64)
import Data.Binary (Word64)
import Data.Binary.Put
import Data.ByteString.Lazy hiding (pack, length, map, groupBy)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as BS
import qualified Text.Search.Sphinx.Types as T

num     = putWord32be . fromIntegral
num64 i = putWord64be $ fromIntegral i

enum :: Enum a => a -> Put
enum = num . fromEnum

list f ls = num (length ls) >> mapM_ f ls
numList   ls = list num ls
numList64 ls = list num64 ls

nums   cfg = mapM_ (\x -> num   $ x cfg)
num64s cfg = mapM_ (\x -> num64 $ x cfg)

stringIntList :: [(String, Int)] -> Put
stringIntList xs = list strInt xs
 where strInt (s,i) = str s >> num i

str :: String -> Put
str s = do let bs = pack s
           num (fromEnum $ BS.length bs)
           putLazyByteString bs

cmd :: T.SearchdCommand -> Put
cmd = putWord16be . toEnum . T.searchdCommand

verCmd :: T.VerCommand -> Put
verCmd = putWord16be . toEnum . T.verCommand
