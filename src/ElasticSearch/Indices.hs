module ElasticSearch.Indices where
import ElasticSearch.Types

setIndex :: ElasticSearchM ()
setIndex = undefined

deleteIndex :: ElasticSearchM ()
deleteIndex = undefined

checkIndicesExist :: ElasticSearchM ()
checkIndicesExist = undefined

openIndex :: ElasticSearchM ()
openIndex = undefined

closeIndex :: ElasticSearchM ()
closeIndex = undefined

setMapping :: ElasticSearchM ()
setMapping = undefined

getMapping :: ElasticSearchM ()
getMapping = undefined

deleteMapping :: ElasticSearchM ()
deleteMapping = undefined

checkTypesExist :: ElasticSearchM ()
checkTypesExist = undefined

performAliasActions :: ElasticSearchM ()
performAliasActions = undefined

getIndexAlias :: ElasticSearchM ()
getIndexAlias = undefined

getIndexAliases :: ElasticSearchM ()
getIndexAliases = undefined

updateIndexSettings :: ElasticSearchM ()
updateIndexSettings = undefined

getIndexSettings :: ElasticSearchM ()
getIndexSettings = undefined

analyzeText :: ElasticSearchM ()
analyzeText = undefined

setIndexTemplate :: ElasticSearchM ()
setIndexTemplate = undefined

deleteIndexTemplate :: ElasticSearchM ()
deleteIndexTemplate = undefined

getIndexTemplate :: ElasticSearchM ()
getIndexTemplate = undefined

setWarmer :: ElasticSearchM ()
setWarmer = undefined

deleteWarmer :: ElasticSearchM ()
deleteWarmer = undefined

getWarmer :: ElasticSearchM ()
getWarmer = undefined

getIndexStatus :: ElasticSearchM ()
getIndexStatus = undefined

getIndexStatistics :: ElasticSearchM ()
getIndexStatistics = undefined

getIndexSegments :: ElasticSearchM ()
getIndexSegments = undefined

clearIndexCache :: ElasticSearchM ()
clearIndexCache = undefined

flushIndex :: ElasticSearchM ()
flushIndex = undefined

refreshIndex :: ElasticSearchM ()
refreshIndex = undefined

optimizeIndex :: ElasticSearchM ()
optimizeIndex = undefined

gatewaySnapshot :: ElasticSearchM ()
gatewaySnapshot = undefined
