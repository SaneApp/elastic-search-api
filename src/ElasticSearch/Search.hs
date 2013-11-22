module ElasticSearch.Search where
import Data.Aeson (Object)
import ElasticSearch.Types
import ElasticSearch.Search.Types

search :: [Index] -> ElasticSearchM ()
search = undefined

multiSearch :: ElasticSearchM ()
multiSearch = undefined

count :: ElasticSearchM ()
count = undefined

validate :: ElasticSearchM ()
validate = undefined

explain :: ElasticSearchM ()
explain = undefined

percolate :: ElasticSearchM ()
percolate = undefined

moreLikeThis :: ElasticSearchM ()
moreLikeThis = undefined
