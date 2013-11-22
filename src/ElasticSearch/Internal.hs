module ElasticSearch.Internal where
import qualified API as API
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.Types
import Data.ByteString.Char8 (ByteString, pack)
import Data.Text (Text)
import ElasticSearch.Types
import Network.HTTP.Conduit
import Network.HTTP.Types

get :: FromJSON a => String -> ElasticSearchM (Maybe a)
get r = ElasticSearchM $ do
  resp <- API.get $ pack r
  return $ case responseStatus resp == notFound404 of
    False -> parseMaybe parseJSON $ responseBody resp
    True -> Nothing

put :: (ToJSON a, FromJSON b) => String -> a -> ElasticSearchM b
put r x = ElasticSearchM $ do
  resp <- API.put (pack r) x
  return $ responseBody resp

post :: (ToJSON a, FromJSON b) => String -> a -> ElasticSearchM b
post r x = ElasticSearchM $ do
  resp <- API.post (pack r) x
  return $ responseBody resp

delete :: FromJSON a => String -> ElasticSearchM a
delete r = ElasticSearchM $ do
  resp <- API.delete (pack r) ()
  return $ responseBody resp

runElasticSearch :: String -> ElasticSearchM a -> IO (Either API.APIError a)
runElasticSearch baseUrl = API.runAPIClient baseUrl elasticSearchMiddleware . fromElasticSearchM

elasticSearchMiddleware r = r { checkStatus = \_ _ _ -> Nothing }