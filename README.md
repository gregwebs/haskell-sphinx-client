A haskell implementation of a [sphinx full text search](http://sphinxsearch.com) client.
Sphinx is a very fast and featureful full-text search daemon.
Version 0.4 is Compatible with sphinx version 1.1-beta
Version 0.5 is Compatible with sphinx version 2.0-beta, but you can pass the version-one-one build flag.
[On hackage](http://hackage.haskell.org/package/sphinx).

Usage
-----
`query` executes a single query.
`runQueries` executes multiple queries at once that were created them with `addQuery`

In extended mode you may want to escape special query characters with `escapeString`

`buildExcerpts` creates highlighted excerpts

You will probably need to import the types also:

    import qualified Text.Search.Sphinx as Sphinx
    import qualified Text.Search.Sphinx.Types as SphinxT

There is also an `Indexable` module for generating an xml file of data to be indexed

Details
=======

Implemenation
-------------
Implementation of API as detailed in [the documentation](http://sphinxsearch.com/docs/manual-1.10.html).
Most search and buildExcerpts features are implemented.

History
-------
Originally written by Tupil and maintained by Chris Eidhof for an earlier version of sphinx.
Greg Weber updated the library for the latest version of sphinx and is now maintaining it.

Usage of this haskell client
----------------------------
Tupil originally wrote this for use on a commercial project.
This sphinx package is now finding some use in the Yesod community. [Here is a well described example usage](http://www.yesodweb.com/book/sphinx), but do keep in mind there is no requirement to tie the *generation* of sphinx documents to your web application, just your database. Used in Yesod applications yesdoweb.com and eatnutrients.com.
