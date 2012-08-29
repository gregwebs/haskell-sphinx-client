module Text.Search.Sphinx.Indexable (
  SchemaType (..), Id, 
  SphinxSchema (..), serialize
  )
  where

import Data.Text (unpack)
import qualified Text.Search.Sphinx.Types as T

--import Text.Search.Sphinx.Types
import Text.XML.Light

data SchemaType = TField
                | TAttribute T.AttrT
                | TFieldString

type Id = Int

class SphinxSchema a where
  -- | Convert a value of a to a document with a document id and some attributes and fields.
  toDocument :: a -> (Id, [(String, T.Attr)])
  -- | The first parameter should be ignored, but is used to satisfy Haskell's type system.
  schema   :: a -> [(String, SchemaType)]

serialize :: SphinxSchema a => [a] -> Element
serialize items =
  sphinxEl "docset" << (
      sphinxEl "schema" << (map schemaField $ schema (head $ items))
    : map (doc . toDocument) items
  )

doc :: (Id, [(String, T.Attr)]) -> Element
doc (id, fields) = sphinxEl "document" ! [("id", show id)] <<
                     map docEl fields

docEl :: (String, T.Attr) -> Element
docEl (name, content) = normalEl name `text` indexableEl content

indexableEl (T.AttrUInt i)   = simpleText $ show i
indexableEl (T.AttrString s) = simpleText $ unpack s
indexableEl (T.AttrFloat f)  = simpleText $ show f
indexableEl _  = error "not implemented"

simpleText s = CData { cdVerbatim = CDataText
                     , cdData     = s
                     , cdLine     = Nothing
                     }

schemaField :: (String, SchemaType) -> Element
schemaField (name, TField)       = sphinxEl "field" ! [("name", name)]
schemaField (name, TAttribute t) = sphinxEl "attr" ! [("name", name), ("type", attrType t)]
schemaField (name, TFieldString) = sphinxEl "field_string" ! [("name", name), ("type", attrType T.AttrTString)]

attrType :: T.AttrT -> String
attrType T.AttrTString      = "string"
attrType T.AttrTStr2Ordinal = "str2ordinal"
attrType T.AttrTUInt        = "int"
attrType T.AttrTFloat       = "float"
attrType _       = error "not implemented"

text :: Element -> CData -> Element
text el dat = el {elContent = [Text dat]}

(<<) :: Element -> [Element] -> Element
a << b = a {elContent = map Elem b}

(!) :: Element -> [(String, String)] -> Element
el ! attrs = el {elAttribs = [Attr (unqual name) value | (name, value) <- attrs]}

sphinxEl :: String -> Element
sphinxEl name = Element { elName    = sphinxNm name
                        , elAttribs = []
                        , elContent = []
                        , elLine    = Nothing
                        }

normalEl :: String -> Element
normalEl name = Element { elName    = unqual name
                        , elAttribs = []
                        , elContent = []
                        , elLine    = Nothing
                        }

sphinxNm name = blank_name { qPrefix = Just "sphinx"
                           , qURI    = Nothing
                           , qName   = name
                           }
