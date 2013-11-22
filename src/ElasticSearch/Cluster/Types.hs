module ElasticSearch.Cluster.Types where


data Node = NodeId | Local | Address | Attribute | Name

data HealthStatusOptions = HealthStatusOptions
  { level :: Cluster | Indices | Shards
  , waitForStatus :: Green | Yellow | Red
  , waitForRelocatingShards :: Int
  , waitForNodes :: Range
  , timeout :: TimeSpan
  }

data HealthStatus = HealthStatus
  { _healthstatusClusterName :: Text
  , _healthstatusStatus :: HealthStatus
  , _healthstatusTimedOut :: Bool
  , _healthstatusNumberOfNodes :: Int
  , _healthstatusNumberOfDataNodes :: Int
  , _healthstatusActivePrimaryShards :: Int
  , _healthstatusActiveShards :: Int
  , _healthstatusRelocatingShards :: Int
  , _healthstatusInitializingShards :: Int
  , _healthstatusUnassignedShards :: Int
  }
{-
{
 : "testcluster",
 : "green",
 : false,
 : 2,
 : 2,
 : 5,
 : 10,
 : 0,
 : 0,
 : 0
} 
-}

