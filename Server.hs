-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Main where

import           Data.Aeson               (ToJSON)
import           Data.Functor             ((<&>))
import           Data.Maybe               (fromMaybe)
import           GHC.Generics             (Generic)
import           Network.Wai.Handler.Warp (run)
import           Servant                  (Application, Get, JSON, Proxy (..),
                                           Server, serve, type (:<|>) (..),
                                           type (:>))
import           Servant.API              (Capture)
import           System.Environment       (lookupEnv)
import           Text.Read                (readMaybe)

type BaseAPI   = Capture "value" String :> Get '[JSON] Response
type ReallyAPI = "isItReally3" :> BaseAPI
type API       = BaseAPI :<|> ReallyAPI :<|> Get '[JSON] Response

data Response = Response { yep :: Bool, message :: String } deriving Generic

instance ToJSON Response

server :: Server API
server = go :<|> go :<|> pure (Response False "Give me a number")
    where
        go  = pure . uncurry Response . go'
        go' = (readMaybe :: String -> Maybe Integer) <&> \case
                Just 3  -> (True,  "Yep, it's 3")
                Just _  -> (False, "Not 3")
                Nothing -> (False, "Y not integer")

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = do
    let defaultPort = 33333
    port <- fromMaybe defaultPort . (readMaybe =<<) <$> lookupEnv "PORT"
    run port app
