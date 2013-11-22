{-# LANGUAGE QuasiQuotes #-}
module ElasticSearch.Document where
import Control.Lens ((^.))
import Data.Aeson (ToJSON, FromJSON)
import Data.Default
import Data.Text (unpack)
import ElasticSearch.Internal
import ElasticSearch.Types
import ElasticSearch.Document.Types
import URI.TH
import URI.Types

putDocument :: ToJSON a => Index -> Type -> a -> ElasticSearch IndexPutOptions IndexPutResponse
putDocument i t x f = case fmap unpack $ opts ^. id' of
  Just mid -> put [uri| /{i}/{t}{/mid}{?opts} |] x
  Nothing -> post [uri| /{i}/{t}{?opts} |] x
  where
    opts :: IndexPutOptions
    opts = f def

getDocument :: FromJSON a => Index -> Id -> ElasticSearch IndexGetOptions (Maybe (IndexGetResponse a))
getDocument i ident f = get [uri| /{i}/{mt}/{ident}{?opts} |]
  where
    mt = maybe "_all" unpack $ opts ^. type'
    opts :: IndexGetOptions
    opts = f def

deleteDocument :: Index -> Type -> Id -> ElasticSearch IndexDeleteOptions IndexDeleteResponse
deleteDocument i t id f = delete [uri| /{i}/{t}/{id}{?opts} |]
  where
    opts :: IndexDeleteOptions
    opts = f def

updateDocument :: Index -> Type -> Id -> IndexUpdate -> ElasticSearch IndexUpdateOptions IndexUpdateResponse
updateDocument i t id u f = post [uri| /{i}/{t}/{id}/_update{?opts} |] u
  where
    opts :: IndexUpdateOptions
    opts = f def

-- performBulkDocumentActions :: [BulkDocumentOperation] -> ElasticSearchM BulkOperationResponse

-- deleteDocumentsByQuery
