module Text.Search.Sphinx.Get where

import Data.Binary.Get
import Data.Int (Int64)
import Prelude hiding (readList)
import Data.ByteString.Lazy hiding (pack, length, map, groupBy)
import Control.Monad
import qualified Text.Search.Sphinx.Types as T
import Data.Maybe (isJust, fromJust)

import Debug.Trace
debug a = trace (show a) a

-- Utility functions
getNum :: Get Int
getNum = getWord32be >>= return . fromEnum

getNum64 :: Get Int64
getNum64 = getWord64be >>= return . fromIntegral

readList f = do num <- getNum
                num `times` f
times = replicateM

getStr = do len <- getNum
            getLazyByteString (fromIntegral len)

getResult :: Get (T.QueryResult)
getResult = do
  statusNum <- getNum
  case T.toQueryStatus statusNum of
    T.QueryERROR n -> do e <- getStr
                         return $ T.QueryError statusNum e
    T.QueryOK      -> getResultOk >>= return . T.QueryOk
    T.QueryWARNING -> do w <- getStr
                         getResultOk >>= return . (T.QueryWarning w)
  where
    getResultOk = do
      fields     <- readList getStr
      attrs      <- readList readAttrPair
      matchCount <- getNum
      id64       <- getNum
      matches    <- matchCount `times` readMatch (id64 > 0) (map snd attrs)
      [total, totalFound, time, numWords] <- 4 `times` getNum
      wrds       <- numWords `times` readWord
      return $ T.SearchResult matches total totalFound wrds (map fst attrs)


readWord = do s <- getStr
              [doc, hits] <- 2 `times` getNum
              return (s, doc, hits)

readMatch isId64 attrs = do
    doc <- if isId64 then getNum64 else (getNum >>= return . fromIntegral)
    weight <- getNum
    matchAttrs <- mapM readAttr attrs
    return $ T.Match doc weight matchAttrs
  where
    readAttr (T.AttrTMulti attr)  = (readList (readAttr attr)) >>= return . T.AttrMulti
    readAttr T.AttrTBigInt    = getNum64 >>= return . T.AttrBigInt
    readAttr T.AttrTString    = getStr  >>= return . T.AttrString
    readAttr T.AttrTUInt      = getNum >>= return . T.AttrUInt
    readAttr T.AttrTFloat     = error "readAttr for AttrFloat not implemented yet."
    readAttr _                = getNum  >>= return . T.AttrUInt


readAttrPair = do
    s <- getStr
    t <- getNum
    return (s, toEnum t)

readHeader = runGet $ do status  <- getWord16be
                         version <- getWord16be
                         length  <- getWord32be
                         return (T.toStatus $ fromIntegral status, version, length)
