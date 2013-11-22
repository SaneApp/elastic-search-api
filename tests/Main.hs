{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where
import API (APIError)
import Control.Lens
import Control.Applicative
import Data.Aeson
import Data.Maybe
import GHC.Generics
import Test.Hspec
import ElasticSearch.Document
import ElasticSearch.Document.Types
import ElasticSearch.Internal
import ElasticSearch.Types

es :: ElasticSearchM a -> IO a
es m = do
  (Right r) <- runElasticSearch "http://192.168.33.10:9200" m
  return r

main = hspec $ do
  describe "get nonexistent document" $ do
    it "returns nothing" $ do
      es getNotFound `shouldReturn` True
  describe "put and get document" $ do
    it "should return the stored document" $ do
      es setAndGet `shouldReturn` True
  describe "put and delete document" $ do
    it "should be retrievable before deletion, but not after" $ do
      es setAndGet `shouldReturn` True
      es (deleteDocument "test" "test" "test_object" id) >>= \p -> shouldSatisfy p (^. found)
      es getNotFound `shouldReturn` True
  describe "update document" $ do
    it "should return the updated document" $ do
      es setAndGet `shouldReturn` True
      let updateCommand = UpdateWithScript "ctx._source.test = \"updated\"" Nothing Nothing
      es (updateDocument "test" "test" "test_object" updateCommand id) >>= \p -> shouldSatisfy p (^. ok)
      es (testGet "test_object" id) >>= \(Just r) ->  fmap test ((r :: IndexGetResponse TestObject) ^. source') `shouldBe` Just "updated"

data TestObject = TestObject { test :: String }
  deriving (Eq, Generic, Show)

instance ToJSON TestObject
instance FromJSON TestObject

testGet :: FromJSON a => Id -> ElasticSearch IndexGetOptions (Maybe (IndexGetResponse a))
testGet = getDocument "test"

testPut :: TestObject -> ElasticSearch IndexPutOptions IndexPutResponse
testPut = putDocument "test" "test"

setAndGet :: ElasticSearchM Bool
setAndGet = do
  let testObject = TestObject "this is a test"
  let withTestId = id' ?~ "test_object"
  testPut testObject withTestId
  obj <- testGet "test_object" (type' ?~ "test")
  case obj of
    Nothing -> return False
    Just o -> return (o ^. source' == Just testObject)

getNotFound :: ElasticSearchM Bool
getNotFound = isNothing <$> do
  res <- testGet "does_not_exist" (type' ?~ "test")
  return (res :: Maybe (IndexGetResponse TestObject))


