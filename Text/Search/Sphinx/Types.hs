module Text.Search.Sphinx.Types (
    module Text.Search.Sphinx.Types
  , ByteString ) where

import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import Data.Maybe (Maybe, isJust)

-- | Search commands
data SearchdCommand = ScSearch
                    | ScExcerpt
                    | ScUpdate
                    | ScKeywords
                    deriving (Show, Enum)

searchdCommand :: SearchdCommand -> Int
searchdCommand = fromEnum

-- | Current client-side command implementation versions
data VerCommand = VcSearch
                | VcExcerpt
                | VcUpdate
                | VcKeywords
                deriving (Show)

-- | Important! Search is updated to 1.1, others have been left unchanged
verCommand VcSearch   = 0x117
verCommand VcExcerpt  = 0x100
verCommand VcUpdate   = 0x101
verCommand VcKeywords = 0x100

-- | Searchd status codes
data StatusCode = OK
                | ERROR
                | RETRY
                | WARNING
                deriving (Show)

toEnumStatus 0 = OK
toEnumStatus 2 = RETRY
toEnumStatus 3 = WARNING
toEnumStatus n = ERROR

-- | Match modes
data MatchMode = All
               | Any
               | Phrase
               | Boolean
               | Extended
               | Fullscan
               | Extended2  -- extended engine V2 (TEMPORARY, WILL BE REMOVED)
               deriving (Show, Enum)

-- | Ranking modes (ext2 only)
data Rank = ProximityBm25  -- default mode, phrase proximity major factor and BM25 minor one
          | Bm25           -- statistical mode, BM25 ranking only (faster but worse quality)
          | None           -- no ranking, all matches get a weight of 1
          | WordCount      -- simple word-count weighting, rank is a weighted sum of per-field keyword occurence counts
          deriving (Show, Enum)

-- | Sort modes
data Sort = Relevance
          | AttrDesc
          | AttrAsc
          | TimeSegments
          | SortExtended -- constructor already existed
          | Expr
          deriving (Show, Enum)

-- | Filter types
data Filter = ExclusionFilter Filter
            | FilterValues String [Int64]
            | FilterRange  String Int64 Int64
            -- TODO | FilterFloatRange attr Float Float
            deriving (Show)

-- | shortcut for creating an exclusion filter
exclude filter = ExclusionFilter filter

fromEnumFilter (FilterValues _ _)  = 0
fromEnumFilter (FilterRange _ _ _) = 1
-- fromEnumFilter (FilterFloatRange _ _ _) = 2

-- | Attribute types
data AttrT = AttrTUInt          -- unsigned 32-bit integer
           | AttrTTimestamp     -- timestamp
           | AttrTStr2Ordinal   -- ordinal string number (integer at search time, specially handled at indexing time)
           | AttrTBool          -- boolean bit field
           | AttrTFloat         -- floating point number (IEEE 32-bit)
           | AttrTBigInt        -- signed 64-bit integer
           | AttrTString        -- string (binary; in-memory)
           | AttrTWordCount     -- string word count (integer at search time,tokenized and counted at indexing time)
           | AttrTMulti AttrT   -- multiple values (0 or more) 
           deriving (Show)

instance Enum AttrT where
    toEnum = toAttrT
    fromEnum = attrT

toAttrT 1          = AttrTUInt
toAttrT 2          = AttrTTimestamp
toAttrT 3          = AttrTStr2Ordinal
toAttrT 4          = AttrTBool
toAttrT 5          = AttrTFloat
toAttrT 6          = AttrTBigInt
toAttrT 7          = AttrTString
toAttrT 8          = AttrTWordCount
toAttrT 0x40000001 = AttrTMulti AttrTUInt

attrMultiMask = 0x40000000

attrT AttrTUInt        = 1
attrT AttrTTimestamp   = 2
attrT AttrTStr2Ordinal = 3
attrT AttrTBool        = 4
attrT AttrTFloat       = 5
attrT AttrTBigInt      = 6
attrT AttrTString      = 7
attrT AttrTWordCount   = 8
attrT (AttrTMulti AttrTUInt) = 0x40000001

-- | Grouping functions
data GroupByFunction = Day
                     | Week
                     | Month
                     | Year
                     | Attr
                     | AttrPair
                     deriving (Show, Enum)

-- | The result of a query
data SearchResult = SearchResult {
      -- | The matches
      matches :: [Match]
      -- | Total amount of matches retrieved on server by this query.
    , total   :: Int
      -- | Total amount of matching documents in index.
    , totalFound :: Int
      -- | processed words with the number of docs and the number of hits.
    , words :: [(ByteString, Int, Int)]
      -- | list of attribute names. The Match will contain just the attribute values.
    , attributeNames :: [ByteString]
}
 deriving Show

-- | a single result, used internally for Results
data Result = ResultOk SearchResult
            | ResultWarning ByteString SearchResult
            | ResultError Int ByteString
            deriving (Show)

-- | multiple results from runQueries
data Results = ResultsOk [Result]
             | ResultsWarning ByteString [Result]
             | ResultsError Int ByteString
             | ResultsRetry ByteString
             deriving (Show)

-- | a single result returned from the query function
-- | a case of runQuries where there is just 1 query
-- | avoids extra unwrapping for the end user of query
data QueryResult = Ok SearchResult
             | Warning ByteString SearchResult
             | Error Int ByteString
             | Retry ByteString
             deriving (Show)

data Match = Match {
             -- Document ID
               documentId :: Int64
             -- Document weight
             , documentWeight :: Int
             -- Attribute values
             , attributeValues :: [Attr]
             }
 deriving Show

data Attr = AttrMulti [Attr]
          | AttrUInt  Int
          | AttrBigInt Int64
          | AttrString ByteString
 deriving Show
