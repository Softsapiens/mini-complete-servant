{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Lib
    ( startApp
    ) where

import           Api                         (app)
import           Config                      (Config (..), Environment (..),
                                              defaultConfig, makePool,
                                              setLogger)
import           Database.Persist.Postgresql (runSqlPool)
import           Models
import           Network.Wai
import           Network.Wai.Handler.Warp    (run)
import           Servant
import           System.Environment          (lookupEnv)

type API = "users" :> Get '[JSON] [User]

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a

startApp :: IO ()
startApp = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8081
    pool <- makePool env
    let cfg = defaultConfig { getPool = pool, getEnv = env }
        logger = setLogger env
    runSqlPool doMigrations pool
    run port $ logger $ app cfg
{-
app :: Config -> Application
app c = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "Isaac" "Newton" "isaac@cambridge.uk"
        , User 2 "Albert" "Einstein" "albert@mit.edu"
        ]
-}
