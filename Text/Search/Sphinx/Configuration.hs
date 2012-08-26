module Text.Search.Sphinx.Configuration where

import qualified Text.Search.Sphinx.Types as T

-- | The configuration for a query
--
-- A note about encodings: The encoding specified here is used to encode
-- every @Text@ value that is sent to the server, and it used to decode all
-- of the server's answers, including error messages.
--
-- If the specified encoding doesn't support characters sent to the server,
-- they will silently be substituted with the byte value of @\'\\SUB\' ::
-- 'Char'@ before transmission.
--
-- If the server sends a byte value back that the encoding doesn't understand,
-- the affected bytes will be converted into special values as
-- specified by that encoding. For example, when decoding invalid UTF-8,
-- all invalid bytes are going to be substituted with @\'\\65533\' ::
-- 'Char'@.
--
data Configuration = Configuration {
    -- | The hostname of the Sphinx daemon
    host :: String
    -- | The portnumber of the Sphinx daemon
  , port :: Int
    -- | Encoding used to encode queries to the server, and decode server responses
  , encoding :: String
    -- | Per-field weights
  , weights :: [Int]
    -- | How many records to seek from result-set start (default is 0)
  , offset :: Int
    -- | How many records to return from result-set starting at offset (default is 20)
  , limit :: Int
    -- | Query matching mode
  , mode :: T.MatchMode
    -- | Ranking mode
  , ranker :: T.Rank
    -- | Match sorting mode
  , sort :: T.Sort
    -- | Attribute to sort by
  , sortBy :: String
    -- | Minimum ID to match, 0 means no limit
  , minId :: Int
    -- | Maximum ID to match, 0 means no limit
  , maxId :: Int
    -- | attribute filters
  , filters :: [T.Filter]
    -- | Group-by sorting clause (to sort groups in result set with)
  , groupBy :: String
    -- | Group-by count-distinct attribute
  , groupSort :: String
    -- | Group-by function (to pre-process group-by attribute value with)
  , groupByFunc :: T.GroupByFunction
    -- | Group-by attribute name 
  , groupDistinct :: String
    -- | Maximum number of matches to retrieve
  , maxMatches :: Int
    -- | Cutoff to stop searching at
  , cutoff :: Int
    -- | Distributed retries count
  , retryCount :: Int
    -- | Distributed retries delay
  , retryDelay :: Int
    -- | Per-index weights
  , indexWeights :: [(String, Int)]
    -- | Maximum query time in milliseconds, 0 means no limit
  , maxQueryTime :: Int
    -- | Per-field-name weights
  , fieldWeights :: [(String, Int)]
    -- | attributes to select, defaults to \"*\"
  , selectClause :: String -- setSelect in regular API
}
 deriving (Show)

-- | A basic, default configuration.
defaultConfig = Configuration {
                  port          = 3312
                , host          = "127.0.0.1"
                , encoding      = "UTF-8"
                , weights       = []
                , offset        = 0
                , limit         = 20
                , mode          = T.All
                , ranker        = T.ProximityBm25
                , sort          = T.Relevance
                , sortBy        = ""
                , minId         = 0
                , maxId         = 0
                , filters       = []
                , groupSort     = "@group desc"
                , groupBy       = ""
                , groupByFunc   = T.Day
                , groupDistinct = ""
                , maxMatches    = 1000
                , cutoff        = 0
                , retryCount    = 0
                , retryDelay    = 0
                , indexWeights  = []
                , maxQueryTime  = 0
                , fieldWeights  = []
                , selectClause  = "*"
              }
