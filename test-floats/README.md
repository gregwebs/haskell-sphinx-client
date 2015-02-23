# sphinx-test

A test of the FilterFloatRange option for Text.Sphinx.Search.

## Setup

Run  `./setup.sh` to create and populate the DB.

The script assumes you have a MySQL user named root without a password. If this
is wrong you can run the steps in the script manually, and you should also edit
sphinx.conf accordingly.

The values in the database are

    +----+-------+--------+--------+
    | id | title | price  | rating |
    +----+-------+--------+--------+
    |  1 | cake  |  23.99 |    3.4 |
    |  2 | soda  |   1.99 |    4.4 |
    |  3 | cake  | 123.99 |    2.4 |
    +----+-------+--------+--------+

Spin up the sphinx daemon

    searchd --config sphinx.conf

Create the sphinx index:

    indexer --config sphinx.conf  --all --rotate

Then:  
    
    cabal sandbox init
    cabal install --only-dependences
    cabal build

## Run the test

Run the simple test runner:

```
dist/build/sphinx-test/sphinx-test
```

Result:

> Ok (QueryResult {matches = [Match {documentId = 2, documentWeight = 1, attributeValues = [AttrFloat 1.99,AttrFloat 4.4]}], total = 1, totalFound = 1, words = [], attributeNames = ["price","rating"]})


The test searches without a match string with the filter:

    [FilterFloatRange "price" 1.72 3.10]

This appears to work.

