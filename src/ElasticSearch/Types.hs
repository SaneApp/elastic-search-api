{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, Rank2Types, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module ElasticSearch.Types where
import API
import Control.Applicative
import Control.Monad
import Control.Lens (to, (^.), Getter)
import Control.Lens.TH
import Data.Aeson
import Data.Maybe
import Data.Text (Text, unpack, intercalate)
import Data.Time.Clock
import Data.Functor.Identity
import URI.Types

data VoidF f = VoidF
newtype ElasticSearchM a = ElasticSearchM { fromElasticSearchM :: APIClient a }
  deriving (Monad, Functor)
type ElasticSearch q r = (q -> q) -> ElasticSearchM r
type Id = Text
type Index = Text
type Type = Text

data Percolate = Percolate
  deriving (Show)

type TimeSpan = DiffTime

data OpType = Create
  deriving (Show)

data Consistency = One | Quorum | All
  deriving (Show)

data ReplicationType = Async | Sync
  deriving (Show)

data Fields = Source | Fields [Text]
  deriving (Show)

data ShardPreference = Primary | Local | Custom Text
  deriving (Show)

instance ToTemplateValue Bool SingleElement where
  toTemplateValue True = Single "true"
  toTemplateValue False = Single "false"

instance ToTemplateValue Text SingleElement where
  toTemplateValue = Single . unpack

instance ToTemplateValue ReplicationType SingleElement where
  toTemplateValue Async = Single "async"
  toTemplateValue Sync = Single "sync"

instance ToTemplateValue Consistency SingleElement where
  toTemplateValue One = Single "one"
  toTemplateValue Quorum = Single "quorum"
  toTemplateValue All = Single "all"

instance ToTemplateValue OpType SingleElement where
  toTemplateValue Create = Single "create"

instance ToTemplateValue Percolate SingleElement where
  toTemplateValue Percolate = Single "*"

instance ToTemplateValue UTCTime SingleElement where
  toTemplateValue = Single . show

instance ToTemplateValue DiffTime SingleElement where
  toTemplateValue = Single . show

instance ToTemplateValue Fields SingleElement where
  toTemplateValue Source = Single "_source"
  toTemplateValue (Fields fs) = Single $ unpack $ intercalate "," fs

instance ToTemplateValue ShardPreference SingleElement where
  toTemplateValue Primary = Single "primary"
  toTemplateValue Local = Single "local"
  toTemplateValue (Custom c) = Single $ unpack c

val :: (ToTemplateValue l SingleElement) => String -> Getter s (Maybe l) -> s -> Maybe (String, TemplateValue SingleElement)
val str l x = maybe Nothing (\v -> Just (str, v)) mv
  where mv = fmap toTemplateValue (x ^. l)

mkAssoc x = Associative . catMaybes . map ($ x)

data ElasticSearchError = ElasticSearchError
  { _eseStatus :: Int
  , _eseError' :: Text
  } deriving (Show)

makeFields ''ElasticSearchError

instance FromJSON ElasticSearchError where
  parseJSON (Object v) = ElasticSearchError <$> v .: "status" <*> v .: "error"
  parseJSON _ = mzero
