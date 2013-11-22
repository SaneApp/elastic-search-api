{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, Rank2Types, OverloadedStrings #-}
module ElasticSearch.Document.Types where
import Control.Applicative
import Control.Lens ((^.))
import Control.Lens.TH
import Control.Monad
import Data.Aeson
import Data.Default
import Data.Text (Text, unpack)
import Data.Time (UTCTime)
import ElasticSearch.Types
import URI.Types

data IndexPutResponse = IndexPutResponse
  { _ixprOk       :: Bool
  , _ixprIndex'   :: Text
  , _ixprType'    :: Text
  , _ixprId'      :: Text
  , _ixprVersion' :: Int
  } deriving (Show)

instance FromJSON IndexPutResponse where
  parseJSON (Object v) = IndexPutResponse <$>
    v .: "ok" <*>
    v .: "_index" <*>
    v .: "_type" <*>
    v .: "_id" <*>
    v .: "_version"
  parseJSON _ = mzero

data IndexPutOptions = IndexPutOptions
  { _ixpoVersion     :: Maybe Int
  , _ixpoOpType      :: Maybe OpType
  , _ixpoRouting     :: Maybe Text
  , _ixpoParent      :: Maybe Id
  , _ixpoTimestamp   :: Maybe UTCTime
  , _ixpoTtl         :: Maybe Int
  , _ixpoPercolate   :: Maybe Percolate
  , _ixpoConsistency :: Maybe Consistency
  , _ixpoReplication :: Maybe ReplicationType
  , _ixpoRefresh     :: Maybe Bool
  , _ixpoTimeout     :: Maybe TimeSpan
  , _ixpoId'         :: Maybe Id
  } deriving (Show)

instance Default IndexPutOptions where
  def = IndexPutOptions def def def def def def def def def def def def

data IndexGetOptions = IndexGetOptions
  { _ixgoRealtime   :: Maybe Bool
  , _ixgoFields     :: Maybe Fields
  , _ixgoRouting    :: Maybe Text
  , _ixgoPreference :: Maybe ShardPreference
  , _ixgoRefresh    :: Maybe Bool
  , _ixgoType'      :: Maybe Type
  } deriving (Show)

instance Default IndexGetOptions where
  def = IndexGetOptions def def def def def def

data IndexGetResponse a = IndexGetResponse
  { _ixgrIndex'  :: Index
  , _ixgrType'   :: Type
  , _ixgrId'     :: Id
  , _ixgrVersion :: Maybe Int
  , _ixgrExists  :: Bool
  , _ixgrSource' :: Maybe a
  , _ixgrFields  :: Maybe a
  } deriving (Show)

instance FromJSON a => FromJSON (IndexGetResponse a) where
  parseJSON (Object v) = IndexGetResponse <$>
    v .: "_index" <*>
    v .: "_type" <*>
    v .: "_id" <*>
    v .:? "_version" <*>
    v .: "exists" <*>
    v .:? "_source" <*>
    v .:? "fields"
  parseJSON _ = mzero

data IndexDeleteOptions = IndexDeleteOptions
  { _ixdoVersion     :: Maybe Int
  , _ixdoRouting     :: Maybe Text
  , _ixdoParent      :: Maybe Id
  , _ixdoReplication :: Maybe ReplicationType
  , _ixdoConsistency :: Maybe Consistency
  , _ixdoRefresh     :: Maybe Bool
  } deriving (Show)

instance Default IndexDeleteOptions where
  def = IndexDeleteOptions def def def def def def

data IndexDeleteResponse = IndexDeleteResponse
  { _ixdrOk     :: Bool
  , _ixdrIndex' :: Index
  , _ixdrType'  :: Type
  , _ixdrId'    :: Id
  , _ixdrFound  :: Bool
  } deriving (Show)

instance FromJSON IndexDeleteResponse where
  parseJSON (Object v) = IndexDeleteResponse <$>
    v .: "ok" <*>
    v .: "_index" <*>
    v .: "_type" <*>
    v .: "_id" <*>
    v .: "found"
  parseJSON _ = mzero

data IndexUpdate
  = UpdateWithScript
    { _ixuScript :: Text
    , _ixuParams :: Maybe Value
    , _ixuUpsert :: Maybe Value
    }
  | UpdatePartial
    { _ixuDoc :: Value
    , _ixuDocAsUpsert :: Bool
    } deriving (Show)

instance ToJSON IndexUpdate where
  toJSON (UpdateWithScript script param upsert) = object [ "script" .= script, "param" .= param, "upsert" .= upsert]
  toJSON (UpdatePartial doc docAsUpsert) = object [ "doc" .= doc, "doc_as_upsert" .= docAsUpsert ]

data IndexUpdateOptions = IndexUpdateOptions
  { _ixuoRouting         :: Maybe Text
  , _ixuoParent          :: Maybe Id
  , _ixuoTimeout         :: Maybe TimeSpan
  , _ixuoReplication     :: Maybe ReplicationType
  , _ixuoConsistency     :: Maybe Consistency
  , _ixuoPercolate       :: Maybe Percolate
  , _ixuoRefresh         :: Maybe Bool
  -- , _ixuoFields          :: Maybe Fields
  , _ixuoRetryOnConflict :: Maybe Int
  } deriving (Show)

instance Default IndexUpdateOptions where
  def = IndexUpdateOptions def def def def def def def def -- def

data IndexUpdateResponse = IndexUpdateResponse
  { _ixurOk       :: Bool
  , _ixurIndex'   :: Text
  , _ixurType'    :: Text
  , _ixurId'      :: Text
  , _ixurVersion' :: Int  
  } deriving (Show)

instance FromJSON IndexUpdateResponse where
  parseJSON (Object v) = IndexUpdateResponse <$>
    v .: "ok" <*>
    v .: "_index" <*>
    v .: "_type" <*>
    v .: "_id" <*>
    v .: "_version"

data GetDoc = GetDoc
  { _gdIndex' :: Index
  , _gdType'  :: Type
  , _gdId'    :: Id
  , _gdFields :: Maybe [Text]
  } deriving (Show)

data MultiGetRequest
  = AllIndices [GetDoc]
  | SingleIndex Index [GetDoc]
  | SingleType Index Type [GetDoc]

makeFields ''IndexPutResponse
makeFields ''IndexPutOptions
makeFields ''IndexGetOptions
makeFields ''IndexGetResponse
makeFields ''IndexDeleteOptions
makeFields ''IndexDeleteResponse
makeFields ''IndexUpdate
makeFields ''IndexUpdateOptions
makeFields ''IndexUpdateResponse
makeFields ''GetDoc

instance ToTemplateValue IndexPutOptions AssociativeListElement where
  toTemplateValue x = mkAssoc x
    [ val "version" version
    , val "op_type" opType
    , val "routing" routing
    , val "parent" parent
    , val "timestamp" timestamp
    , val "ttl" ttl
    , val "percolate" percolate
    , val "consistency" consistency
    , val "replication" replication
    , val "refresh" refresh
    , val "timeout" timeout
    ]

instance ToTemplateValue IndexGetOptions AssociativeListElement where  
  toTemplateValue x = mkAssoc x
    [ val "realtime" realtime
    , val "fields" fields
    , val "routing" routing
    , val "preference" preference
    , val "refresh" refresh
    ]

instance ToTemplateValue IndexDeleteOptions AssociativeListElement where  
  toTemplateValue x = mkAssoc x
    [ val "version" version
    , val "routing" routing
    , val "parent" parent
    , val "consistency" consistency
    , val "refresh" refresh
    ]

instance ToTemplateValue IndexUpdateOptions AssociativeListElement where
  toTemplateValue x = mkAssoc x
    [ val "routing" routing
    , val "parent" parent
    , val "timeout" timeout
    , val "replication" replication
    , val "consistency" consistency
    , val "percolate" percolate
    , val "refresh" refresh
    -- , val "fields" fields
    , val "retry_on_conflict" retryOnConflict
    ]
