module Text.Search.Sphinx.Get where

import Data.Binary.Get
import Data.Int (Int64)
import Prelude hiding (readList)
import Data.ByteString.Lazy hiding (pack, length, map, groupBy)
import Control.Monad
import qualified Text.Search.Sphinx.Types as T
import Data.Maybe (isJust, fromJust)

-- Utility functions
getNum :: Get Int
getNum = getWord32be >>= return . fromEnum

getNum64 :: Get Int64
getNum64 = getWord64be >>= return . fromIntegral

getNums = readList getNum
readList f = do num <- getNum
                num `times` f
times = replicateM
readField = getStr

getStr = do len <- getNum
            getLazyByteString (fromIntegral len)


getResult :: Get T.Result
getResult = do
  statusNum <- getNum
  (warning, error) <- case T.toEnumStatus statusNum of
                        T.OK      -> return (Nothing, Nothing)
                        T.WARNING -> do w <- getStr
                                        return (Just $ T.ResultWarning w, Nothing)
                        T.ERROR -> do e <- getStr
                                      return (Nothing, Just $ T.ResultError statusNum e)
  (if isJust error
    then return $ (fromJust error)
    else do
      fields     <- readList readField
      attrs      <- readList readAttr
      matchCount <- getNum
      id64       <- getNum
      matches    <- matchCount `times` readMatch (id64 > 0) (map snd attrs)
      [total, totalFound, time, numWords] <- 4 `times` getNum
      wrds       <- numWords `times` readWord
      let result = T.SearchResult matches total totalFound wrds
      return (if isJust warning then (fromJust warning) result else T.ResultOk result)
      )


readWord = do s <- getStr
              [doc, hits] <- 2 `times` getNum
              return (s, doc, hits)

readMatch isId64 attrs = do
    doc <- if isId64 then getNum64 else (getNum >>= return . fromIntegral)
    weight <- getNum
    matchAttrs <- mapM readMatchAttr attrs
    return $ T.Match doc weight matchAttrs

readMatchAttr (T.AttrTMulti T.AttrTUInt)  = getNums >>= return . T.AttrMulti
readMatchAttr (T.AttrTMulti t) = error $ "readMatchAttr not implemented for MVA " ++ show t ++ " yet."
readMatchAttr T.AttrTFloat     = error "readMatchAttr for AttrFloat not implemented yet."
readMatchAttr _                           = getNum  >>= return . T.AttrNum

readAttr = do
    s <- getStr
    t <- getNum
    return (s, toEnum t)

readHeader = runGet $ do status  <- getWord16be
                         version <- getWord16be
                         length  <- getWord32be
                         return (status, version, length)
