module Text.Search.Sphinx.ExcerptConfiguration where

-- import qualified Text.Search.Sphinx.Types as T

data ExcerptConfiguration = ExcerptConfiguration {
    -- | The hostname of the Sphinx daemon
    host :: String
    -- | The portnumber of the Sphinx daemon
  , port :: Int
    -- | Encoding used to encode queries to the server, and decode server responses
  , encoding :: String
  , beforeMatch :: String
  , afterMatch :: String
  , chunkSeparator :: String
  , limit  :: Int
  , around :: Int
  , exactPhrase :: Bool
  , singlePassage :: Bool
  , useBoundaries :: Bool
  , weightOrder :: Bool
  -- | warning! broken on 1.10-beta (keep to default of false). Fixed on trunk
  , queryMode :: Bool
  , forceAllWords :: Bool
  , limitPassages :: Int
  , limitWords :: Int
  , startPassageId :: Int
  , loadFiles :: Bool
  , htmlStripMode :: String
  , allowEmpty :: Bool
  , passageBoundary :: String
}
 deriving (Show)

-- this is true to the API
defaultConfig = ExcerptConfiguration {
    port          = 3312
  , host          = "127.0.0.1"
  , encoding      = "utf8"
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
  , htmlStripMode = "index" -- "none", "strip", "index", and "retain". 
  , allowEmpty = False
  , passageBoundary = "none"
}

-- this seems better to me
altConfig = defaultConfig {
    beforeMatch = "<span class='match'>"
  , afterMatch = "</span>"
  , chunkSeparator = " &#8230; "
  -- , queryMode = True Buggy!
  , forceAllWords = True
}
