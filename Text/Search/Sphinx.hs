-- The following functions are not yet implemented:
-- setFilterFloatRange, setGeoAnchor
-- resetFilters, resetGroupBy
-- buildExcerpts, updateAttributes, buildKeyWords
-- escapeString, status, open, close
module Text.Search.Sphinx ( module Text.Search.Sphinx,
  Configuration(..), defaultConfig ) where

import qualified Text.Search.Sphinx.Types as T (VerCommand(VcSearch),
  SearchdCommand(ScSearch), SearchResult, Results(..), Result(..), Filter, Filter(..),
  fromEnumFilter, Filter(..), StatusCode(..), toEnumStatus)

import Text.Search.Sphinx.Configuration (Configuration(..), defaultConfig)
import Text.Search.Sphinx.Get (times, getResult, readHeader, getStr)
import Text.Search.Sphinx.Put (num, num64, enum, list, numList, numList64, nums,
                              num64s, stringIntList, str, cmd, verCmd)

import Data.Binary.Put (Put, runPut)
import Data.Binary.Get (runGet, getWord32be)
import qualified Data.ByteString.Lazy as BS (ByteString, length, hGet, hPut, tail)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Int (Int64)

import Network (connectTo, PortID(PortNumber))
import IO (Handle, hFlush)

import Prelude hiding (filter, tail)

import Debug.Trace
debug a = trace (show a) a

setFilter :: Configuration -> String -> [Int64] -> Bool -> Configuration
setFilter cfg attr values exclude =
  let f = (T.FilterValues attr values)
  in  addFilter cfg (if exclude then T.ExclusionFilter f else f)

setFilterRange :: Configuration -> String -> Int64 -> Int64 -> Bool -> Configuration
setFilterRange cfg attr min max exclude =
  let f = (T.FilterRange attr min max)
  in  addFilter cfg (if exclude then T.ExclusionFilter f else f)

--setFilterFloatRange :: Configuration -> String -> Float -> Float -> Bool -> Configuration
--setFilterFloatRange cfg attr min max exclude =
  --let f = (T.FilterFloatRange attr min max)
  --in  addFilter cfg (if exclude then T.ExclusionFilter f else f)

-- | alternative interface to setFilter* using Filter constructors
addFilter :: Configuration -> T.Filter -> Configuration
addFilter cfg filter = cfg { filters = filter : (filters cfg) }

addQuery :: Configuration -> String -> String -> String -> Put
addQuery cfg query index comment = do
    nums cfg [ offset
             , limit
             , fromEnum . mode
             , fromEnum . ranker
             , fromEnum . sort]
    str (sortBy cfg)
    str query
    numList (weights cfg)
    str index
    num 1                     -- id64 range marker
    num64s cfg [minId, maxId] -- id64 range

    list putFilter (filters cfg)

    enum (groupByFunc   cfg)
    str  (groupBy       cfg)
    num  (maxMatches    cfg)
    str  (groupSort     cfg)
    num  (cutoff        cfg)
    num  (retryCount    cfg)
    num  (retryDelay    cfg)
    str  (groupDistinct cfg)
    num 0 -- anchor point for setGeoAnchor
    stringIntList (indexWeights cfg)
    num (maxQueryTime cfg)
    stringIntList (fieldWeights cfg)
    str comment
    -- attribute overrides
    -- select-list
    where
      putFilter :: T.Filter -> Put
      putFilter (T.ExclusionFilter filter) = putFilter_ filter True
      putFilter                     filter = putFilter_ filter False
      putFilter_ f@(T.FilterValues attr values)  ex = putFilter__ f attr numList64 [values] ex
      putFilter_ f@(T.FilterRange  attr min max) ex = putFilter__ f attr num64 [min, max] ex
      putFilter__ filter attr puter values exclude = do
        str (debug attr)
        num $ (debug $ T.fromEnumFilter filter)
        mapM_ puter (debug values)
        num $ (debug $ fromEnum exclude)


-- | The 'query' function queries the Sphinx daemon.
query :: Configuration -- ^ The configuration
      -> String        -- ^ The indexes, "*" means every index
      -> String        -- ^ The query string
      -> IO (T.Results)
query config indexes s = do
    conn <- connect config
    let q = addQuery config s indexes ""
    results <- runQueries conn q 1
    return results
  where
    connect :: Configuration -> IO Handle
    connect cfg = do connection <- connectTo (host cfg) (PortNumber $ fromIntegral $ port cfg)
                     bs         <- BS.hGet connection 4
                     let version   = runGet getWord32be bs
                         myVersion = runPut (num 1)
                     BS.hPut connection myVersion
                     return connection

runQueries :: Handle -> Put -> Int -> IO (T.Results)
runQueries conn q numQueries = do
    let req = runPut (makeRunQuery q numQueries)
    BS.hPut conn req
    hFlush conn
    getResponse conn numQueries
  where 
    makeRunQuery query numQueries = do
      cmd T.ScSearch
      verCmd T.VcSearch
      num $ fromEnum $ BS.length (runPut query) + 4
      num numQueries
      query

    getResponse :: Handle -> Int -> IO (T.Results)
    getResponse conn numResults = do
      header <- BS.hGet conn 8
      let x@(status, version, len) = readHeader header
      if len == 0
        then error "received zero-sized searchd response (bad query?)"
        else return ()

      response  <- BS.hGet conn (fromIntegral len)

      case T.toEnumStatus status of
        T.OK      -> return $ T.Ok (getResults response)
        T.WARNING -> return $ T.Warning (runGet getStr response) (getResults response)
        T.RETRY   -> return $ T.Retry (errorMessage response)
        T.ERROR   -> return $ T.Error (fromIntegral status) (errorMessage response)
      where
        getResults response = runGet (numResults `times` getResult) response
        errorMessage response = BS.tail (BS.tail (BS.tail (BS.tail response)))
