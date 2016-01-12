{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Control.Monad.Reader        (ReaderT, lift, runReaderT)
import           Control.Monad.Trans.Either  (EitherT, left)
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectList, (==.))
import           Network.Wai                 (Application)
import           Servant

import           Config                      (Config (..))
import           Models

type UserAPI =
         "users" :> Get '[JSON] [User]
    :<|> "users" :> Capture "email" String :> Get '[JSON] User
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

type AppM = ReaderT Config (EitherT ServantErr IO)

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Config -> Application
app cfg = serve userAPI (readerServer cfg)

readerServer :: Config -> Server UserAPI
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

server :: ServerT UserAPI AppM
server = allUsers :<|> singleUser :<|> createUser

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
         []     -> lift $ left err404
         (x:xs) -> return x

createUser :: User -> AppM Int64
createUser p = do
    newUser <- runDb $ insert $ Users (ident p) (firstName p) (lastName p) (email p)
    return $ fromSqlKey newUser
