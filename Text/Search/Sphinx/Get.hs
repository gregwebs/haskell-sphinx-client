module Text.Search.Sphinx.Get where

import Data.Binary.Get
import Data.Int (Int64)
import Prelude hiding (readList)
import Data.ByteString.Lazy hiding (pack, length, map, groupBy)
import Control.Monad
import Text.Search.Sphinx.Types

-- Utility functions
getNum :: Get Int
getNum = getWord32be >>= return . fromEnum

getNum64 :: Get Int64
getNum64 = getWord64be >>= return . fromIntegral

getNums = readList getNum
readList f = do num <- getNum
                num `times` f
times = replicateM
readField = readStr
readStr = do len <- getNum
             getLazyByteString (fromIntegral len)


getResult :: Get SearchResult
getResult = do
    status     <- getNum
    -- todo: we suppose the status is OK
    fields     <- readList readField
    attrs      <- readList readAttr
    matchCount <- getNum
    id64       <- getNum
    matches    <- matchCount `times` readMatch (id64 > 0) (map snd attrs)
    [total, totalFound, time, numWords] <- 4 `times` getNum
    wrds       <- numWords `times` readWord
    return (SearchResult matches total totalFound wrds)


readWord = do s <- readStr
              [doc, hits] <- 2 `times` getNum
              return (s, doc, hits)

readMatch isId64 attrs = do
    doc <- if isId64 then getNum64 else (getNum >>= return . fromIntegral)
    weight <- getNum
    matchAttrs <- mapM readMatchAttr attrs
    return $ Match doc weight matchAttrs

readMatchAttr AttrTFloat  = error "readMatchAttr for AttrFloat not implemented yet."
readMatchAttr AttrTMulti  = getNums >>= return . AttrMulti
readMatchAttr _           = getNum  >>= return . AttrNum

readAttr = do
    s <- readStr
    t <- getNum
    return (s, toEnum t)

readHeader = runGet $ do status  <- getWord16be
                         version <- getWord16be
                         length  <- getWord32be
                         return (status, version, length)
