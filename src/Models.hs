{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Config
import           Control.Monad.Reader        (ReaderT, asks, liftIO)
import           Data.Aeson                  (ToJSON)
import           Data.Aeson.TH               (defaultOptions, deriveJSON)
import           Data.Text
import           Database.Persist.Postgresql (SqlBackend (..), runMigration,
                                              runSqlPool)
import           Database.Persist.TH         (mkMigrate, mkPersist,
                                              persistLowerCase, share,
                                              sqlSettings)
import           GHC.Generics                (Generic)

-- Database table schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Users
    ident Int
    firstName String
    lastName String
    email String
    deriving Show
|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

data User = User
      { ident     :: Int
      , firstName :: String
      , lastName  :: String
      , email     :: String
      } deriving (Eq, Show, Generic)

-- Generates both 'ToJSON' and 'FromJSON' instance declarations for the given
-- data type
$(deriveJSON defaultOptions ''User)

usersToUser :: Users -> User
usersToUser Users{..} = User { ident = usersIdent
                             , firstName = usersFirstName
                             , lastName = usersLastName
                             , email = usersEmail }

newtype PublicData = PublicData { somedata :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData
