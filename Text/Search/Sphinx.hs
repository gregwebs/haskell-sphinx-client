-- | This is the Haskell version of the Sphinx searchd client.

module Text.Search.Sphinx ( Configuration (..)
                          , query
                          , defaultConfig
                          ) where

import Network
import IO hiding (bracket)
import System
import Control.Exception
import Data.Binary.Get
import Data.Binary.Put (runPut, Put)
import Data.ByteString.Lazy hiding (pack, length, map, groupBy, head)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as BS
import Data.Char (ord, chr)
import Data.Int (Int64)
import Prelude hiding (readList)
import Text.Search.Sphinx.Get
import Text.Search.Sphinx.Put
import Text.Search.Sphinx.Configuration
import Text.Search.Sphinx.Types (SearchResult)
import qualified Text.Search.Sphinx.Types as T


type Connection = (Handle, Configuration)


connect :: Configuration -> IO Connection
connect cfg = do connection <- connectTo (host cfg) (PortNumber $ fromIntegral $ port cfg)
                 bs         <- hGet connection 4
                 let version   = runGet getWord32be bs
                     myVersion = runPut (num 1)
                 hPut connection myVersion
                 return (connection, cfg)


addQuery :: Configuration -> String -> String -> String -> Put
addQuery cfg query index comment = do
    nums cfg [ offset
             , limit
             , T.matchMode . mode
             , T.rank      . ranker
             , T.sort      . sort]
    str (sortBy cfg)
    str query
    numList (weights cfg)
    str index
    num 1
    num64s cfg [minId, maxId]
    num 0 -- todo: pack len filters + filters
    enum (groupByFunc   cfg)
    str  (groupBy       cfg)
    num  (maxMatches    cfg)
    str  (groupSort     cfg)
    num  (cutoff        cfg)
    num  (retryCount    cfg)
    num  (retryDelay    cfg)
    str  (groupDistinct cfg)
    num 0 -- anchor point: todo
    stringIntList (indexWeights cfg)
    num (maxQueryTime cfg)
    stringIntList (fieldWeights cfg)
    str comment

-- | The 'query' function queries the Sphinx daemon.
query :: Configuration -- ^ The configuration
      -> String        -- ^ The indexes, "*" means every index
      -> String        -- ^ The query string
      -> IO SearchResult
query config indexes s = do
    conn <- connect config
    let q = addQuery config s indexes ""
    results <- runQueries (fst conn) q 1
    return $ head results -- We only do one query, so we always have one SearchResult

runQueries :: Handle -> Put -> Int -> IO [SearchResult]
runQueries conn q numQueries = do
    let req = runPut (makeRunQuery q numQueries)
    hPut conn req
    hFlush conn
    getResponse conn numQueries

makeRunQuery query numQueries =  do
  cmd T.ScSearch
  verCmd T.VcSearch
  num $ fromEnum $ BS.length (runPut query) + 4
  num numQueries
  query

getResponse conn numResults = do
  header <- hGet conn 8
  let x@(status, version, len) = readHeader header
  response <- hGet conn (fromIntegral len)
  return $ runGet (numResults `times` getResult) response
