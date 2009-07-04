module Text.Search.Sphinx.Indexable (Indexable (..), SchemaType (..), AttrType (..), Id, 
                                     SphinxSchema (..), serialize)
                                     where

--import Text.Search.Sphinx.Types
import Text.XML.Light

-- TODO: this should really be the same as Types.Attr
data Indexable = NumAttr Int
               | StrAttr String
               | Field   String

data SchemaType = TField
                | TAttribute AttrType

data AttrType = AString | AInt

type Id = Int

class SphinxSchema a where
  -- | Convert a value of a to a document with a document id and some attributes and fields.
  toDocument :: a -> (Id, [(String, Indexable)])
  -- | The first parameter should be ignored, but is used to satisfy Haskell's type system.
  schema   :: a -> [(String, SchemaType)]

serialize :: SphinxSchema a => [a] -> Element
serialize items =
  sphinxEl "docset" << (
      sphinxEl "schema" << (map schemaField $ schema (head $ items))
    : map (doc . toDocument) items
  )

doc :: (Id, [(String, Indexable)]) -> Element
doc (id, fields) = sphinxEl "document" ! [("id", show id)] <<
                     map docEl fields

docEl :: (String, Indexable) -> Element
docEl (name, content) = normalEl name `text` indexableEl content

indexableEl (NumAttr i) = simpleText $ show i
indexableEl (StrAttr f) = simpleText $ f
indexableEl (Field f)   = simpleText $ f

simpleText s = CData { cdVerbatim = CDataText
                     , cdData     = s
                     , cdLine     = Nothing
                     }

schemaField :: (String, SchemaType) -> Element
schemaField (name, TField)       = sphinxEl "field" ! [("name", name)]
schemaField (name, TAttribute t) = sphinxEl "attr" ! [("name", name), ("type", attrType t)]

attrType :: AttrType -> String
attrType AString = "str2ordinal"
attrType AInt    = "int"

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
