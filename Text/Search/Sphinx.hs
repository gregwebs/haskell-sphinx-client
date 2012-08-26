{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- The following functions are not yet implemented:
-- setFilterFloatRange, setGeoAnchor
-- resetFilters, resetGroupBy
-- updateAttributes,
-- buildKeyWords, escapeString, status, open, close
module Text.Search.Sphinx 
  ( escapeString
  , escapeText
  , query
  , buildExcerpts
  , runQueries
  , runQueries'
  , resultsToMatches
  , maybeQueries
  , T.Query(..), simpleQuery
  , Configuration(..), defaultConfig
  ) where

import qualified Text.Search.Sphinx.Types as T (
  Match,
  Query(..),
  VerCommand(VcSearch, VcExcerpt),
  SearchdCommand(ScSearch, ScExcerpt),
  Filter, Filter(..),
  fromEnumFilter, Filter(..),
  QueryStatus(..), toStatus, Status(..),
  SingleResult(..), Result(..), QueryResult(..))

import Text.Search.Sphinx.Configuration (Configuration(..), defaultConfig)
import qualified Text.Search.Sphinx.ExcerptConfiguration as ExConf (ExcerptConfiguration(..))
import Text.Search.Sphinx.Get (times, getResult, readHeader, getStr, getTxt)
import Text.Search.Sphinx.Put (num, num64, enum, list, numC, strC, foldPuts,
                              numC64, stringIntList, str, txt, cmd, verCmd)

import Data.Binary.Put (Put, runPut)
import Data.Binary.Get (runGet, getWord32be)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Int (Int64)

import Network (connectTo, PortID(PortNumber))
import System.IO (Handle, hFlush)
import Data.Bits ((.|.))

import Prelude hiding (filter, tail)
import Data.List (nub)

import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Text.ICU.Convert as ICU

{- the funnest way to debug this is to run the same query with an existing working client and look at the difference
 - sudo tcpflow -i lo dst port 9306 
import Debug.Trace; debug a = trace (show a) a
-}

escapedChars :: String
escapedChars =  '"':'\\':"-!@~/()*[]="

-- | escape all possible meta characters.
--   most of these characters only need to be escaped in certain contexts
--   however, in normal searching they will all be ignored
escapeString :: String -> String
escapeString [] = []
escapeString (x:xs) = if x `elem` escapedChars
                        then '\\':x:escapeString xs
                        else      x:escapeString xs

escapeText :: Text -> Text
escapeText = X.concatMap (\x -> if x `elem` escapedChars
                                 then X.pack $ '\\':[x]
                                 else X.singleton x)

-- | The 'query' function runs a single query against the Sphinx daemon.
--   To pipeline multiple queries in a batch, use addQuery and runQueries
query :: Configuration -- ^ The configuration
      -> String        -- ^ The indexes, \"*\" means every index
      -> Text        -- ^ The query string
      -> IO (T.Result T.QueryResult) -- ^ just one search result back
query config indexes search = do
    let q = T.Query search indexes X.empty
    results <- runQueries' config [q]
    -- same as toSearchResult, but we know there is just one query
    -- could just remove and use runQueries in the future
    return $ case results of
      T.Ok rs -> case head rs of
                        T.QueryOk result        -> T.Ok result
                        T.QueryWarning w result -> T.Warning w result
                        T.QueryError code e     -> T.Error code e
      T.Error   code error      -> T.Error code error
      T.Retry   retry           -> T.Retry retry
      T.Warning warning (result:results) -> case result of
                        T.QueryOk result        -> T.Warning warning result
                        T.QueryWarning w result -> T.Warning (BS.append warning w) result
                        T.QueryError code e     -> T.Error code e

-- | Prepare a commentless query over all indexes
simpleQuery :: Text  -- ^ The query string
            -> T.Query -- ^ A query value that can be sent to 'runQueries'
simpleQuery q = T.Query q "*" X.empty

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
              -> [Text]               -- ^ list of document contents to be highlighted
              -> String                 -- ^ The indexes, \"*\" means every index
              -> Text                  -- ^ The query string to use for excerpts
              -> IO (T.Result [Text]) -- ^ the documents with excerpts highlighted
buildExcerpts config docs indexes words = do
  conn <- connect (ExConf.host config) (ExConf.port config)
  conv <- ICU.open (ExConf.encoding config) Nothing
  let req = runPut $ makeBuildExcerpt (addExcerpt conv)
  BS.hPut conn req
  hFlush conn
  (status, response) <- getResponse conn
  case status of
    T.OK      -> return $ T.Ok (getResults response conv)
    T.WARNING -> return $ T.Warning (runGet getStr response) (getResults response conv)
    T.RETRY   -> return $ T.Retry (errorMessage response)
    T.ERROR n -> return $ T.Error n (errorMessage response)
  where
    getResults response conv = runGet ((length docs) `times` getTxt conv) response
    errorMessage response = BS.tail (BS.tail (BS.tail (BS.tail response)))

    makeBuildExcerpt putExcerpt = do
      cmd    T.ScExcerpt
      verCmd T.VcExcerpt  
      num $ fromEnum $ BS.length (runPut putExcerpt)
      putExcerpt

    addExcerpt :: ICU.Converter -> Put
    addExcerpt conv = do
      num 0 -- mode
      num $ excerptFlags config
      str indexes
      txt conv words
      strC config [ExConf.beforeMatch, ExConf.afterMatch, ExConf.chunkSeparator]
      numC config [ExConf.limit, ExConf.around, ExConf.limitPassages, ExConf.limitWords, ExConf.startPassageId]
      str $ ExConf.htmlStripMode config
#ifndef ONE_ONE_BETA
      str $ ExConf.passageBoundary config
#endif
      list (txt conv) docs

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


-- | Make multiple queries at once, using a list of 'T.Query'.
-- For a single query, just use the query method
-- Easier handling of query result than runQueries'
runQueries :: Configuration -> [T.Query] -> IO (T.Result [T.QueryResult])
runQueries cfg qs = runQueries' cfg qs >>= return . toSearchResult
  where
    --   with batched queries, each query can have an error code,
    --     regardless of the error code given for the entire batch
    --   in general there isn't a reason for a valid query to return an error or warning
    --   using this could make it harder to debug the situation at hand
    --   perform the following conveniences:
    --   * return an Error Result if any SingleResult has an Error status
    --   * pull out any inner warnings to the top level Warning Result
    --     - this compresses all warnings into one which making debugging harder
    toSearchResult :: T.Result [T.SingleResult] -> T.Result [T.QueryResult]
    toSearchResult results =
        case results of
          T.Ok rs              -> fromOk rs [] BS.empty
          T.Warning warning rs -> fromWarn warning rs []
          T.Retry   retry      -> T.Retry retry
          T.Error   code error -> T.Error code error
      where
        fromOk :: [T.SingleResult] -> [T.QueryResult] -> BS.ByteString -> T.Result [T.QueryResult]
        fromOk [] acc warn | BS.null warn = T.Ok acc
                           | otherwise = T.Warning warn acc
        fromOk (r:rs) acc warn = case r of
          T.QueryOk result        -> fromOk rs (acc ++ [result]) warn
          T.QueryWarning w result -> fromOk rs (acc ++ [result]) (BS.append warn w)
          T.QueryError code e     -> T.Error code e

        fromWarn :: BS.ByteString -> [T.SingleResult] -> [T.QueryResult] -> T.Result [T.QueryResult]
        fromWarn warning [] acc = T.Warning warning acc
        fromWarn warning (r:rs) acc = case r of
          T.QueryOk result        -> fromWarn warning rs (result:acc)
          T.QueryWarning w result -> fromWarn (BS.append warning w) rs (result:acc)
          T.QueryError code e     -> T.Error code e

-- | lower level- called by 'runQueries'
-- | This may be useful for debugging problems- warning messages won't get compressed
runQueries' :: Configuration -> [T.Query] -> IO (T.Result [T.SingleResult])
runQueries' config qs = do
    conn <- connect (host config) (port config)
    conv <- ICU.open (encoding config) Nothing
    let queryReq = foldPuts $ map (serializeQuery config conv) qs
    BS.hPut conn (request queryReq)
    hFlush conn
    getSearchResult conn conv
  where 
    numQueries = length qs
    request qr = runPut $ do
                cmd T.ScSearch
                verCmd T.VcSearch
                num $ 
#ifdef ONE_ONE_BETA
                      4
#else
                      8
#endif
                        + (fromEnum $ BS.length (runPut qr))
#ifndef ONE_ONE_BETA
                num 0
#endif
                num numQueries
                qr

    getSearchResult :: Handle -> ICU.Converter -> IO (T.Result [T.SingleResult])
    getSearchResult conn conv = do
      (status, response) <- getResponse conn
      case status of
        T.OK      -> return $ T.Ok (getResults response conv)
        T.WARNING -> return $ T.Warning (runGet getStr response) (getResults response conv)
        T.RETRY   -> return $ T.Retry (errorMessage response)
        T.ERROR n -> return $ T.Error n (errorMessage response)
      where
        getResults response conv = runGet (numQueries `times` getResult conv) response
        errorMessage response    = BS.tail (BS.tail (BS.tail (BS.tail response)))


-- | Combine results from 'runQueries' into matches.
resultsToMatches :: Int -> [T.QueryResult] -> [T.Match]
resultsToMatches maxResults = combine
  where
    combine [] = []
    combine (r:rs)
        | T.totalFound r == maxResults = T.matches r
        | T.totalFound r == 0          = combine rs
        | otherwise                          = takeResults (r:rs)
    takeResults :: [T.QueryResult] -> [T.Match]
    takeResults = take maxResults . nub . foldl1 (++) . map T.matches


-- | executes 'runQueries'. Log warning and errors, automatically retry.
-- Return a Nothing on error, otherwise a Just.
maybeQueries :: (BS.ByteString -> IO ()) -> Configuration -> [T.Query] -> IO (Maybe [T.QueryResult])
maybeQueries logCallback conf queries = do
  result <- runQueries conf queries
  case result of
    T.Ok r           -> return (Just r)
    T.Retry msg      -> logCallback msg  >> maybeQueries logCallback conf queries
    T.Warning w r    -> logCallback w    >> return (Just r)
    T.Error code msg ->
      logCallback (BS.concat ["Error code ",BS8.pack $ show code,". ",msg]) >> return Nothing

getResponse :: Handle -> IO (T.Status, BS.ByteString)
getResponse conn = do
  header <- BS.hGet conn 8
  let (status, version, len) = readHeader header
  if len == 0
    then error "received zero-sized searchd response (bad query?)"
    else return ()
  response <- BS.hGet conn (fromIntegral len)
  return (status, response)

-- | use with runQueries to pipeline a batch of queries
serializeQuery :: Configuration -> ICU.Converter -> T.Query -> Put
serializeQuery cfg conv (T.Query qry indexes comment) = do
    numC cfg [ offset
             , limit
             , fromEnum . mode
             , fromEnum . ranker
             , fromEnum . sort]
    str (sortBy cfg)
    txt conv qry
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
    txt conv comment
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
        str attr
        num $ T.fromEnumFilter filter
        mapM_ puter values
        num $ fromEnum exclude

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
