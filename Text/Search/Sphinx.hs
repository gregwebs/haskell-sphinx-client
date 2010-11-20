-- The following functions are not yet implemented:
-- setFilterFloatRange, setGeoAnchor
-- resetFilters, resetGroupBy
-- updateAttributes,
-- buildKeyWords, escapeString, status, open, close
module Text.Search.Sphinx ( module Text.Search.Sphinx
  , Configuration(..), defaultConfig
  ) where

import qualified Text.Search.Sphinx.Types as T (
  VerCommand(VcSearch, VcExcerpt),
  SearchdCommand(ScSearch, ScExcerpt),
  Filter, Filter(..),
  fromEnumFilter, Filter(..),
  QueryStatus(..), toStatus, Status(..),
  SearchResult, Result(..), QueryResult(..))

import Text.Search.Sphinx.Configuration (Configuration(..), defaultConfig)
import qualified Text.Search.Sphinx.ExcerptConfiguration as ExConf (ExcerptConfiguration(..))
import Text.Search.Sphinx.Get (times, getResult, readHeader, getStr)
import Text.Search.Sphinx.Put (num, num64, enum, list, numC, strC,
                              numC64, stringIntList, str, cmd, verCmd)

import Data.Binary.Put (Put, runPut)
import Data.Binary.Get (runGet, getWord32be)
import qualified Data.ByteString.Lazy as BS (ByteString, length, hGet, hPut, tail, append)
import Data.Int (Int64)

import Network (connectTo, PortID(PortNumber))
import IO (Handle, hFlush)
import Data.Bits ((.|.))

import Prelude hiding (filter, tail)

import Debug.Trace
debug a = trace (show a) a

{- weren't working properly, should try out on latest version now
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
  -}

-- | The 'query' function runs a single query against the Sphinx daemon.
query :: Configuration -- ^ The configuration
      -> String        -- ^ The indexes, "*" means every index
      -> String        -- ^ The query string
      -> IO (T.Result T.SearchResult) -- ^ just one search result back
query config indexes s = do
    conn <- connect (host config) (port config)
    let q = addQuery config s indexes ""
    results <- runQueries conn [q] 1
    return $ case results of
      T.Ok rs -> case head rs of -- there is just 1 result
                        T.QueryOk result        -> T.Ok result
                        T.QueryWarning w result -> T.Warning w result
                        T.QueryError code e     -> T.Error code e
      T.Error   code error      -> T.Error code error
      T.Retry   retry           -> T.Retry retry
      T.Warning warning (result:results) -> case result of
                        T.QueryOk result        -> T.Warning warning result
                        T.QueryWarning w result -> T.Warning (BS.append warning w) result
                        T.QueryError code e     -> T.Error code e
         
connect :: String -> Int -> IO Handle
connect host port = do
  connection <- connectTo host (PortNumber $ fromIntegral $ port)
  bs         <- BS.hGet connection 4
  let version   = runGet getWord32be bs
      myVersion = runPut (num 1)
  BS.hPut connection myVersion
  return connection

-- | TODO: add configuration options
buildExcerpts :: ExConf.ExcerptConfiguration -- ^ Contains host and port for connection and optional configuration for buildExcerpts
              -> [String]               -- ^ list of document contents to be highlighted
              -> String                 -- ^ The indexes, "*" means every index
              -> String                 -- ^ The query string to use for excerpts
              -> IO (T.Result [BS.ByteString]) -- ^ the documents with excerpts highlighted
buildExcerpts config docs indexes words = do
  conn <- connect (ExConf.host config) (ExConf.port config)
  let req = runPut $ makeBuildExcerpt addExcerpt
  BS.hPut conn req
  hFlush conn
  (status, response) <- getResponse conn
  case status of
    T.OK      -> return $ T.Ok (getResults response)
    T.WARNING -> return $ T.Warning (runGet getStr response) (getResults response)
    T.RETRY   -> return $ T.Retry (errorMessage response)
    T.ERROR n -> return $ T.Error n (errorMessage response)
  where
    getResults response = runGet ((length docs) `times` getStr) response
    errorMessage response = BS.tail (BS.tail (BS.tail (BS.tail response)))

    makeBuildExcerpt putExcerpt = do
      cmd    T.ScExcerpt
      verCmd T.VcExcerpt  
      num $ fromEnum $ BS.length (runPut putExcerpt)
      putExcerpt

    addExcerpt :: Put
    addExcerpt = do
      num 0 -- mode
      num $ excerptFlags config
      str indexes
      str words
      strC config [ExConf.beforeMatch, ExConf.afterMatch, ExConf.chunkSeparator]
      numC config [ExConf.limit, ExConf.around, ExConf.limitPassages, ExConf.limitWords, ExConf.startPassageId]
      str $ ExConf.htmlStripMode config
      list str docs

    modeFlag :: ExConf.ExcerptConfiguration -> (ExConf.ExcerptConfiguration -> Bool) -> Int -> Int
    modeFlag cfg setting value = if setting cfg then value else 0

    excerptFlags :: ExConf.ExcerptConfiguration -> Int
    excerptFlags cfg = foldl (.|.) 1 (map (\(s,v) -> modeFlag cfg s v) [
        (ExConf.exactPhrase,      2 )
      , (ExConf.singlePassage,    4 )
      , (ExConf.useBoundaries,    8 )
      , (ExConf.weightOrder,     16 )
      , (ExConf.queryMode,       32 )
      , (ExConf.forceAllWords,   64 )
      , (ExConf.loadFiles,      128 )
      , (ExConf.allowEmpty,     256 )
      ])


-- | right now this is just a lower leve method used by the query method
-- | eventually it will be its own entry point that can run multiple queries by using addQuery
-- | right now it just runs the first query and ignores the rest
runQueries :: Handle -> [Put] -> Int -> IO (T.Result [T.QueryResult])
runQueries conn qs numQueries = do
    let nQueries = 1
    let req = runPut $ makeRunQuery (head qs) nQueries
    BS.hPut conn req
    hFlush conn
    getSearchResult conn nQueries
  where 
    makeRunQuery query n = do
      cmd T.ScSearch
      verCmd T.VcSearch
      num $ fromEnum $ BS.length (runPut query) + 4
      num n
      query

getSearchResult :: Handle -> Int -> IO (T.Result [T.QueryResult])
getSearchResult conn numResults = do
  (status, response) <- getResponse conn
  case status of
    T.OK      -> return $ T.Ok (getResults response)
    T.WARNING -> return $ T.Warning (runGet getStr response) (getResults response)
    T.RETRY   -> return $ T.Retry (errorMessage response)
    T.ERROR n -> return $ T.Error n (errorMessage response)
  where
    getResults response = runGet (numResults `times` getResult) response
    errorMessage response = BS.tail (BS.tail (BS.tail (BS.tail response)))

getResponse :: Handle -> IO (T.Status, BS.ByteString)
getResponse conn = do
  header <- BS.hGet conn 8
  let (status, version, len) = readHeader header
  if len == 0
    then error "received zero-sized searchd response (bad query?)"
    else return ()
  response <- BS.hGet conn (fromIntegral len)
  return (status, response)

-- | right now this is just a lower level method used by the query method
-- | when runQueries works properly this is used to run batched queries
addQuery :: Configuration -> String -> String -> String -> Put
addQuery cfg query indexes comment = do
    numC cfg [ offset
             , limit
             , fromEnum . mode
             , fromEnum . ranker
             , fromEnum . sort]
    str (sortBy cfg)
    str query
    list num (weights cfg)
    str indexes
    num 1                     -- id64 range marker
    numC64 cfg [minId, maxId] -- id64 range

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
    num 0 -- attribute overrides (none)
    str (selectClause cfg) -- select-list
    where
      {- Not working properly -}
      putFilter :: T.Filter -> Put
      putFilter (T.ExclusionFilter filter) = putFilter_ filter True
      putFilter filter                     = putFilter_ filter False

      putFilter_ f@(T.FilterValues attr values)  ex = putFilter__ f attr (list num64) [values] ex
      putFilter_ f@(T.FilterRange  attr min max) ex = putFilter__ f attr num64 [min, max] ex

      putFilter__ filter attr puter values exclude = do
        str (debug attr)
        num $ (debug $ T.fromEnumFilter filter)
        mapM_ puter (debug values)
        num $ (debug $ fromEnum exclude)
