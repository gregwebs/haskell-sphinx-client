module Text.Search.Sphinx.Types where

import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)

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

verCommand VcSearch   = 0x113
verCommand VcExcerpt  = 0x100
verCommand VcUpdate   = 0x101
verCommand VcKeywords = 0x100

-- | Searchd status codes
data Searchd = Ok
             | Error
             | Retry
             | Warning
             deriving (Show, Enum)

searchd :: Searchd -> Int
searchd = fromEnum

-- | Match modes
data MatchMode = All
               | Any
               | Phrase
               | Boolean
               | Extended
               | Fullscan
               | Extended2  -- extended engine V2 (TEMPORARY, WILL BE REMOVED)
               deriving (Show, Enum)

matchMode :: MatchMode -> Int
matchMode = fromEnum

-- | Ranking modes (ext2 only)
data Rank = ProximityBm25  -- default mode, phrase proximity major factor and BM25 minor one
          | Bm25           -- statistical mode, BM25 ranking only (faster but worse quality)
          | None           -- no ranking, all matches get a weight of 1
          | WordCount      -- simple word-count weighting, rank is a weighted sum of per-field keyword occurence counts
          deriving (Show, Enum)

rank :: Rank -> Int
rank = fromEnum

-- | Sort modes
data Sort = Relevance
          | AttrDesc
          | AttrAsc
          | TimeSegments
          | SortExtended -- constructor already existed
          | Expr
          deriving (Show, Enum)

sort :: Sort -> Int
sort = fromEnum

-- | Filter types
data Filter = Values
            | Range
            | FloatRange
            deriving (Show, Enum)

filter :: Filter -> Int
filter = fromEnum

-- | Attribute types
data AttrT = AttrTInteger
           | AttrTTimestamp
           | AttrTOrdinal
           | AttrTBool
           | AttrTFloat
           | AttrTMulti
           deriving (Show)

instance Enum AttrT where
    toEnum = toAttrT
    fromEnum = attrT

toAttrT 1          = AttrTInteger
toAttrT 2          = AttrTTimestamp
toAttrT 3          = AttrTOrdinal
toAttrT 4          = AttrTBool
toAttrT 5          = AttrTFloat
toAttrT 0x40000000 = AttrTMulti     

attrT AttrTInteger   = 1
attrT AttrTTimestamp = 2
attrT AttrTOrdinal   = 3
attrT AttrTBool      = 4
attrT AttrTFloat     = 5
attrT AttrTMulti     = 0x40000000

-- | Grouping functions
data GroupByFunction = Day
                     | Week
                     | Month
                     | Year
                     | Attr
                     | AttrPair
                     deriving (Show, Enum)

groupByFunction :: GroupByFunction -> Int
groupByFunction = fromEnum

-- | The result of a query
data SearchResult = SearchResult {
      -- | The matches
      matches :: [Match]
      -- | Total amount of matches retrieved on server by this query.
    , total   :: Int
      -- | Total amount of matching documents in index.
    , totalFound :: Int
      -- | List of processed words with the number of docs and the number of hits.
    , words :: [(ByteString, Int, Int)]
}
 deriving Show

data Match = Match {
             -- Document ID
               documentId :: Int64
             -- Document weight
             , documentWeight :: Int
             -- Attribute values
             , attributeValues :: [Attr]
             }
 deriving Show

data Attr = AttrMulti [Int]
          | AttrNum  Int
 deriving Show
