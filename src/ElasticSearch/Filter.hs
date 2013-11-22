module ElasticSearch.Filter where

data AlwaysCached
data Cached
data NotCached
data Uncacheable

data Filter a where
  

and :: [Filter]-> Filter
and = and' Nothing

and' :: Maybe CacheSetting -> [Filter]-> Filter


bool ::
bool = bool' Nothing

bool' :: Maybe CacheSetting ->


exists :: Text -> Filter
exists = undefined

geoBoundingBox ::
geoBoundingBox = geoBoundingBox' Nothing

geoBoundingBox' :: Maybe CacheSetting ->


geoDistance ::
geoDistance = geoDistance' Nothing

geoDistance' :: Maybe CacheSetting ->


geoDistanceRange ::
geoDistanceRange = geoDistanceRange' Nothing

geoDistanceRange' :: Maybe CacheSetting ->
geoPolygon ::
geoPolygon = geoPolygon' Nothing

geoPolygon' :: Maybe CacheSetting ->
geoShape ::
geoShape = geoShape' Nothing

geoShape' :: Maybe CacheSetting ->
geoHash ::
geoHash = geoHash' Nothing

geoHash' :: Maybe CacheSetting ->
hasChild
hasParent 
ids :: Maybe [Type] -> [Text]
limit :: Int -> Filter
matchAll :: Filter
missing :: Text -> MissingFieldOptions -> Filter
nested ::
nested = nested' Nothing

nested' :: Maybe CacheSetting ->
not :: Filter -> Filter
not = not' Nothing

not' :: Maybe CacheSetting -> Filter -> Filter
numericRange :: [(Text, RangeOptions)] -> Filter
numericRange = numericRange' Nothing

numericRange' :: Maybe CacheSetting -> [(Text, RangeOptions)] -> Filter

or :: [Filter] -> Filter
or = or' Nothing

or' :: Maybe CacheSetting -> [Filter] -> Filter


prefix :: [(Text, Text)] -> Filter
prefix = prefix' Nothing

prefix' :: Maybe CacheSetting -> [(Text, Text)] -> Filter


query :: Query -> Filter
query = query' Nothing

query' :: Maybe CacheSetting -> Query -> Filter


range :: [(Text, RangeOptions)] -> Filter
range = range' Nothing

range' :: Maybe CacheSetting -> [(Text, RangeOptions)] -> Filter


regexp :: [(Text, Text)] -> Filter
regexp = regexp' Nothing

regexp' :: Maybe CacheSetting -> [(Text, Text)] -> Filter


script :: Script -> Filter
script = script' Nothing

script' :: Maybe CacheSetting -> Script -> Filter


term :: [(Text, Text)] -> Filter
term = term' Nothing

term' :: Maybe CacheSetting -> [(Text, Text)] -> Filter


terms :: [(Text, [Text])] -> Maybe TermsExecution -> Filter
terms = terms' Nothing

terms' :: Maybe CacheSetting -> [(Text, [Text])] -> Maybe TermsExecution -> Filter


typeFilter :: Text -> Filter
typeFilter =
