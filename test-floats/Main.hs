{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
module Main where
import Text.Search.Sphinx
import Data.Text (pack)
import Text.Search.Sphinx.Types
import qualified Text.Search.Sphinx.Configuration as SC

runQuery :: [Filter] -> String -> IO (Result QueryResult)
runQuery xs queryString= do
  let sphinxConf = defaultConfig { filters = xs , port = 9315 }
  query sphinxConf "srcIdx" (pack queryString)

main = do
		let filters = [FilterFloatRange "price" 1.72 3.10]
		-- let filters = []
		let queryString = ""
		res <- runQuery filters queryString
		print res

