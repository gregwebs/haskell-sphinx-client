A haskell implementation of a [sphinx full text search](http://sphinxsearch.com) client.
Sphinx is a very fast and featureful full-text search daemon.
Version 0.4 is Compatible with sphinx version 1.1-beta
Version 0.5 is Compatible with sphinx version 2.0-beta, but you can pass the version-one-one build flag.
[On hackage](http://hackage.haskell.org/package/sphinx).

# Usage

## Constructing Queries

The data type `Query` is used to represent queries to the server. It specifies
a search string and the indexes to run the query on, as well as a comment,
which may be the empty string. In order to run a query on all indexes, use
`"*"` in the index field.

The convenience function `query` executes a single query and constructs the
`Query` by itself, so you don't have to.

To execute more than one `Query`, use `runQueries`. Details are below in the
section [*Batch Queries*](#batch-queries). To construct simple queries, you can
also use `simpleQuery :: Text -> Query` which constructs a `Query` over all
indexes. Don't forget that you can use record updates on a `Query`.

In extended mode you may want to escape special query characters with `escapeString`.

All interaction with the server, including sending queries and receiving
results, is based on the `Data.Text` string type. You might therefore want to
enable the `OverloadedStrings` pragma.

## Excerpts and XML Indexes

`buildExcerpts` creates highlighted excerpts.

You will probably need to import the types as well:

    import qualified Text.Search.Sphinx as Sphinx
    import qualified Text.Search.Sphinx.Types as SphinxT

There is also an `Indexable` module for generating an xml file of data to be indexed.

## Batch Queries

You can send more than one query per request to the server (which may enable
server-side query optimization in certain cases. Refer to the
[Sphinx manual](http://sphinxsearch.com/docs/2.0.4/api-func-addquery.html)
for details.) The function `runQueries` pipelines multiple queries together. If you
are trying to combine the results, there are some helpers such as
`maybeQueries` and `resultsToMatches`.

~~~~~~ {.haskell}
      mr <- Sphinx.maybeQueries sphinxLogger sphinxConfig [
                 SphinxT.Query query1 "db1" ""
               , SphinxT.Query query1 "db2" ""
               , SphinxT.Query query2 "db1" ""
               , SphinxT.Query query2 "db2" ""
               ]
      case mr of
        Nothing -> return Nothing
        Just rs -> do
          let combined = Sphinx.resultsToMatches 20 rs
          if null combined
             then return Nothing
             else return $ Just combined
~~~~~~

**A note** for those transitioning from `0.5.*` to `0.6`: the function `addQueries`
has been removed. You can now directly send a list of `Query` to the server by using
`runQueries`, which will handle the serialization for you behind the scenes.

## Encoding

The sphinx server itself does not know about encodings except for the
difference between single-byte encodings and multi-byte encodings. It assumes
that all incoming queries are already properly encoded and matches the raw
bytes it receives; the same holds for the results returned by the server. Hence
the responsibilty for using the proper encoding (and decoding) routines lies
with the caller.

Version 0.6.0 of `haskell-sphinx-client` introduces the `encoding` field in
both the `Configuration` data type and the `ExcerptConfiguration` data type.
The library handles proper encoding and decoding in the background; just
make sure you set the right encoding setting in the configuration!

Details
=======

Implemenation
-------------
Implementation of API as detailed in [the documentation](http://sphinxsearch.com/docs/manual-1.10.html).
Most search and buildExcerpts features are implemented.

History
-------
Originally written by Tupil and maintained by Chris Eidhof for an earlier version of sphinx.
Greg Weber improved the library and updated it for the latest version of sphinx, and is now maintaining it.

Usage of this haskell client
----------------------------
Tupil originally wrote this for use on a commercial project.
This sphinx package is now finding some use in the Yesod community. [Here is a well described example usage](https://github.com/yesodweb/yesod/wiki/Sphinx-Search), but do keep in mind there is no requirement to tie the *generation* of sphinx documents to your web application, just your database. Used in Yesod applications yesdoweb.com and eatnutrients.com.
