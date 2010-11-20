module Text.Search.Sphinx.ExcerptConfiguration where

-- import qualified Text.Search.Sphinx.Types as T

data ExcerptConfiguration = ExcerptConfiguration {
    -- | The hostname of the Sphinx daemon
    host :: String
    -- | The portnumber of the Sphinx daemon
  , port :: Int
  , beforeMatch :: String
  , afterMatch :: String
  , chunkSeparator :: String
  , limit  :: Int
  , around :: Int
  , exactPhrase :: Bool
  , singlePassage :: Bool
  , useBoundaries :: Bool
  , weightOrder :: Bool
  , queryMode :: Bool
  , forceAllWords :: Bool
  , limitPassages :: Int
  , limitWords :: Int
  , startPassageId :: Int
  , loadFiles :: Bool
  , htmlStripMode :: String
  , allowEmpty :: Bool
}
 deriving (Show)

-- this is true to the API
defaultConfig = ExcerptConfiguration {
    port          = 3312
  , host          = "127.0.0.1"
  , beforeMatch = "<b>"
  , afterMatch = "</b>"
  , chunkSeparator = "..."
  , limit  = 256
  , around = 5
  , exactPhrase = False
  , singlePassage = False
  , weightOrder = False
  , queryMode = False
  , forceAllWords = False
  , limitPassages = 0
  , limitWords = 0
  , useBoundaries = False
  , startPassageId = 1
  , loadFiles = False
  , htmlStripMode = "index" -- ^ "none", "strip", "index", and "retain". 
  , allowEmpty = False
}

-- this seems better to me
altConfig = defaultConfig {
    beforeMatch = "<span class='match'>"
  , afterMatch = "</span>"
  , chunkSeparator = " &#8230; "
  , queryMode = True
  , forceAllWords = True
}
