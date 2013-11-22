module ElasticSearch.Cluster where

getClusterHealth :: HealthStatusOptions -> Maybe [Index] -> ElasticSearchM HealthStatus
getClusterState
rerouteCluster
updateClusterSettings
getNodeStatistics
getNodesInfo
getNodesHotThreads :: ElasticSearchM Text
shutdownNodes