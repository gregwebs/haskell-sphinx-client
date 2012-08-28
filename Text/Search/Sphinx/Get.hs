module Text.Search.Sphinx.Get where

import Data.Binary.Get
import Data.Binary.IEEE754

import Data.Int (Int64)
import Prelude hiding (readList)
import Data.ByteString.Lazy hiding (pack, length, map, groupBy)
import Control.Monad
import qualified Text.Search.Sphinx.Types as T
import Data.Maybe (isJust, fromJust)

import qualified Data.Text.ICU.Convert as ICU

-- Utility functions
getNum :: Get Int
getNum = getWord32be >>= return . fromEnum

getFloat :: Get Float
getFloat = getFloat32be

getNum64 :: Get Int64
getNum64 = getWord64be >>= return . fromIntegral

readList f = do num <- getNum
                num `times` f
times = replicateM

getTxt conv = liftM (ICU.toUnicode conv) getStrStr

getStr = do len <- getNum
            getLazyByteString (fromIntegral len)

-- Get a strict 'ByteString'.
getStrStr = do len <- getNum
               getByteString (fromIntegral len)

getResult :: ICU.Converter -> Get (T.SingleResult)
getResult conv = do
  statusNum <- getNum
  case T.toQueryStatus statusNum of
    T.QueryERROR n -> do e <- getTxt conv
                         return $ T.QueryError statusNum e
    T.QueryOK      -> getResultOk >>= return . T.QueryOk
    T.QueryWARNING -> do w <- getTxt conv
                         getResultOk >>= return . (T.QueryWarning w)
  where
    getResultOk = do
      fields     <- readList getStr
      attrs      <- readList readAttrPair
      matchCount <- getNum
      id64       <- getNum
      matches    <- matchCount `times` readMatch (id64 > 0) (map snd attrs) conv
      [total, totalFound, time, numWords] <- 4 `times` getNum
      wrds       <- numWords `times` readWord conv
      return $ T.QueryResult matches total totalFound wrds (map fst attrs)


readWord conv = do
    s <- getStrStr
    [doc, hits] <- 2 `times` getNum
    return (ICU.toUnicode conv s, doc, hits)

readMatch isId64 attrs conv = do
    doc <- if isId64 then getNum64 else (getNum >>= return . fromIntegral)
    weight <- getNum
    matchAttrs <- mapM readAttr attrs
    return $ T.Match doc weight matchAttrs
  where
    readAttr (T.AttrTMulti attr)  = (readList (readAttr attr)) >>= return . T.AttrMulti
    readAttr T.AttrTBigInt    = getNum64 >>= return . T.AttrBigInt
    readAttr T.AttrTString    = getStrStr  >>= return . T.AttrString . ICU.toUnicode conv
    readAttr T.AttrTUInt      = getNum >>= return . T.AttrUInt
    readAttr T.AttrTFloat     = getFloat >>= return . T.AttrFloat
    readAttr _                = getNum  >>= return . T.AttrUInt


readAttrPair = do
    s <- getStr
    t <- getNum
    return (s, toEnum t)

readHeader = runGet $ do status  <- getWord16be
                         version <- getWord16be
                         length  <- getWord32be
                         return (T.toStatus $ fromIntegral status, version, length)
