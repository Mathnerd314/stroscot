{-# LANGUAGE
EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, DerivingStrategies, StandaloneDeriving,
UndecidableInstances, DataKinds, FlexibleInstances,  ScopedTypeVariables, RecordWildCards, LambdaCase,
PackageImports, OverloadedLabels, DeriveGeneric
#-}
module Database where

import Data.Foldable(forM_)
import Data.Traversable(forM, for)
import Control.Monad.IO.Class  (MonadIO,liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Persist.Sql.Util
import Data.ByteString (ByteString)
import Data.Store
import Data.Store.TH
import Types
import UnliftIO.Resource (ResourceT, runResourceT)
import Control.Monad.Trans.Reader (ReaderT, withReaderT, ask)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT, MonadLoggerIO, logWarn, runLoggingT, askLoggerIO)
import Control.Exception
import qualified Data.Text as T
import "cryptonite" Crypto.Hash
import qualified Data.ByteArray as ByteArray

share [mkPersist sqlSettings, mkMigrate "createTables"] [persistLowerCase|
Runs
  run ByteString
  UniqueR run

ThunkNames
  name ThunkName
  UniqueT name

ThunkHashes
  hash ByteString
  UniqueH hash

DatumNames
  name DatumName
  UniqueDN name

DatumValues
  value DatumValue
  UniqueDV value

RunThunks
  run_id RunsId OnDeleteCascade
  thunk_name ThunkNamesId OnDeleteCascade
  thunk_value ThunkHashesId OnDeleteCascade

ThunkReads
  thunk_id ThunkHashesId OnDeleteCascade
  datum_name DatumNamesId OnDeleteCascade
  datum_value DatumValuesId OnDeleteCascade
  Primary thunk_id datum_name

ThunkWrites
  thunk_id ThunkHashesId OnDeleteCascade
  datum_name DatumNamesId OnDeleteCascade
  datum_value DatumValuesId OnDeleteCascade
  Primary thunk_id datum_name

ThunkSync
  thunk_id ThunkHashesId OnDeleteCascade
  sync (SyncPrimitive (Key ThunkNames))
  Primary thunk_id

ThunkParent
  parent ThunkHashesId OnDeleteCascade
  child ThunkNamesId OnDeleteCascade
  UniqueRel child parent
|]

type SyncInDatabase = SyncPrimitive ThunkNamesId

deriving newtype instance Store (BackendKey SqlBackend)
deriving newtype instance Store ThunkNamesId

instance PersistField SyncInDatabase where
    toPersistValue x = PersistByteString (encode x)
    fromPersistValue (PersistByteString bs) = case decode bs of
      Left (PeekException offset msg) -> Left $ T.concat
        ["Exception while reading SyncPrimitive, "
        , T.pack $ show offset
        , " bytes from end: "
        , msg ]
      Right r -> Right r

instance PersistFieldSql SyncInDatabase where
  sqlType _ = SqlBlob

getOrInsertID_E mkUniq mkRec lbl = do
  conn <- ask
  let uniq = mkUniq lbl
      t = entityDef uniq
      sql = T.concat
              [ "SELECT "
              , T.intercalate "," $ dbIdColumns conn t
              , " FROM "
              , connEscapeName conn $ entityDB t
              , " WHERE "
              , T.intercalate " AND " $ map (go . snd) $ persistUniqueToFieldNames uniq]
      go x = connEscapeName conn x `mappend` "=?"
  let uvals = persistUniqueToValues uniq
  rawSql sql uvals >>= \case
    [id] -> pure $ Left id
    _:_ -> error $ "constraint violation: " ++ show (entityHaskell t) ++ " is not unique!"
    [] -> Right <$> insert (mkRec lbl)

getOrInsertID u r l = either id id <$> getOrInsertID_E u r l

getRunID :: (MonadIO m) => ByteString -> ReaderT SqlBackend m RunsId
getRunID = getOrInsertID UniqueR Runs

getThunkNameID :: (MonadIO m) => ThunkName -> ReaderT SqlBackend m ThunkNamesId
getThunkNameID = getOrInsertID UniqueT ThunkNames

getThunkHashID_E :: (MonadIO m) => ByteString -> ReaderT SqlBackend m (Either ThunkHashesId ThunkHashesId)
getThunkHashID_E = getOrInsertID_E UniqueH ThunkHashes

getDatumNameID :: (MonadIO m) => DatumName -> ReaderT SqlBackend m DatumNamesId
getDatumNameID = getOrInsertID UniqueDN DatumNames

getDatumValueID :: (MonadIO m) => DatumValue -> ReaderT SqlBackend m DatumValuesId
getDatumValueID = getOrInsertID UniqueDV DatumValues

writeThunkRecord
  :: MonadIO m =>
     ThunkRecord -> ReaderT SqlBackend m ThunkHashesId
writeThunkRecord tr@(ThunkRecord{..}) = do
  let h = ByteArray.convert (hash (encode tr) :: Digest SHA256)
  getThunkHashID_E h >>= \case
    Left hid -> pure hid
    Right hid -> do
      forM_ readSet $ \(n,v) -> do
        did <- getDatumNameID n
        vid <- getDatumValueID v
        insert (ThunkReads hid did vid)
      forM_ writeSet $ \(n,v) -> do
        did <- getDatumNameID n
        vid <- getDatumValueID v
        insert (ThunkWrites hid did vid)
      syncP <- traverse getThunkNameID syncPrimitive
      insert (ThunkSync hid syncP)
      forM_ syncP $ \tn -> insert (ThunkParent hid tn)
      pure hid

getThunkRecord
  :: (MonadIO m, MonadFail m) =>
     ThunkHashesId -> ReaderT SqlBackend m ThunkRecord
getThunkRecord hid = do
  reads <- selectList [#thunk_id ==. hid] []
  readSet <- forM (map entityVal reads) $ \ThunkReads{..} -> do
    Just (DatumNames dn) <- get thunkReadsDatum_name
    Just (DatumValues dv) <- get thunkReadsDatum_value
    pure (dn,dv)
  writes <- selectList [#thunk_id ==. hid] []
  writeSet <- forM (map entityVal writes) $ \ThunkWrites{..} -> do
    Just (DatumNames dn) <- get thunkWritesDatum_name
    Just (DatumValues dv) <- get thunkWritesDatum_value
    pure (dn,dv)
  Just sp <- get (ThunkSyncKey hid)
  syncPrimitive <- for (thunkSyncSync sp) $ \tnid -> do
    Just tn <- get tnid
    pure (thunkNamesName tn)
  pure $ ThunkRecord {..}

verifyThunkRecord
  :: (MonadIO m, MonadFail m) =>
     ThunkHashesId -> ReaderT SqlBackend m Bool
verifyThunkRecord hid = do
  tr <- getThunkRecord hid
  let h = ByteArray.convert (hash (encode tr) :: Digest SHA256)
  Just h_recorded <- get hid
  pure $ h == thunkHashesHash h_recorded

recordThunkRecord
  :: MonadIO m =>
     RunsId -> ThunkNamesId -> ThunkHashesId -> ReaderT SqlBackend m RunThunksId
recordThunkRecord runid tid hid = insert (RunThunks runid tid hid)


main :: IO ()
main = runResourceT
      . runNoLoggingT
      . withSqliteConn ":memory:"
      . runSqlConn $ do
  runMigration $ do
    createTables
    -- addMigration False "CREATE INDEX IF NOT EXISTS DatumIndex on \"datum\"(\"dname\");"

  run <- getRunID "local"
  r <- getThunkNameID "root"
  r_n <- writeThunkRecord (ThunkRecord [("a","1"),("b","2")] [] die)
  v <- verifyThunkRecord r_n
  liftIO $ print v
  recordThunkRecord run r r_n
  pure ()
