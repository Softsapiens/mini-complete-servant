{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Api where

import           Config                      (Config (..))
import           Control.Monad.Reader        (ReaderT, lift, runReaderT)
import           Control.Monad.Trans.Except  (ExceptT, throwE)
import           Data.ByteString             (ByteString)
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectList, (==.))
import           Models
import           Network.HTTP.Types
import           Network.Wai                 (Application, requestHeaders)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server.Internal


type DBLookup = ByteString -> IO Bool

isGoodCookie :: DBLookup
isGoodCookie = return . (== "good password")

data AuthProtected

instance HasServer rest => HasServer (AuthProtected :> rest) where
  type ServerT (AuthProtected :> rest) m = ServerT rest m

  route Proxy subserver = WithRequest $ \ request ->
    route (Proxy :: Proxy rest) $ addAcceptCheck subserver $ cookieCheck request
      where
        cookieCheck req = case lookup "Cookie" (requestHeaders req) of
            Nothing -> return $ FailFatal err401 { errBody = "Missing auth header" }
            Just v  -> do
              authGranted <- isGoodCookie v
              if authGranted
                then return $ Route ()
                else return $ FailFatal err403 { errBody = "Invalid cookie" }

type AppM = ReaderT Config (ExceptT ServantErr IO)

type TheAPI = AuthProtected :> "users" :> Get '[JSON] [User]
              :<|> AuthProtected :> "users" :> Capture "email" String :> Get '[JSON] User
              :<|> AuthProtected :> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
              :<|> "description" :> Get '[JSON] [PublicData]

api :: Proxy TheAPI
api = Proxy

readerServer :: Config -> Server TheAPI
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: Config -> AppM :~> ExceptT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

server :: ServerT TheAPI AppM
server = allUsers :<|> singleUser :<|> createUser :<|> publicData

app :: Config -> Application
app cfg = serve api (readerServer cfg)

-- Handlers

publicData :: AppM [PublicData]
publicData = return [PublicData "this is a public piece of data"]

allUsers :: AppM [User]
allUsers = do
    users <- runDb $ selectList [] []
    let people = map (\(Entity _ y) -> usersToUser y) users
    return people

singleUser :: String -> AppM User
singleUser str = do
    users <- runDb $ selectList [UsersEmail ==. str] []
    let list = map (\(Entity _ y) -> usersToUser y) users
    case list of
         []     -> lift $ throwE err404
         (x:xs) -> return x

createUser :: User -> AppM Int64
createUser p = do
    newUser <- runDb $ insert $ Users (ident p) (firstName p) (lastName p) (email p)
    return $ fromSqlKey newUser
