module Text.Search.Sphinx.Put where

import Data.Int (Int64)
import Data.Binary (Word64)
import Data.Binary.Put
import Data.ByteString.Lazy hiding (pack, length, map, groupBy)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as BS
import qualified Text.Search.Sphinx.Types as T
import Data.Binary.IEEE754 

import Data.Text (Text)
import qualified Data.Text.ICU.Convert as ICU
import qualified Data.ByteString as Strict (length)

num     = putWord32be . fromIntegral
num64 i = putWord64be $ fromIntegral i

float = putFloat32be 

enum :: Enum a => a -> Put
enum = num . fromEnum

list f ls = num (length ls) >> mapM_ f ls

numC   cfg = mapM_ (\x -> num   $ x cfg)
numC64 cfg = mapM_ (\x -> num64 $ x cfg)
strC   cfg = mapM_ (\x -> str   $ x cfg)

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

foldPuts :: [Put] -> Put
foldPuts [] = return ()
foldPuts [p] = p
foldPuts (p:ps) = p >> foldPuts ps

txt :: ICU.Converter -> Text -> Put
txt conv t = do let bs = ICU.fromUnicode conv t
                num (fromEnum $ Strict.length bs)
                putByteString bs
