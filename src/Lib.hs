module Lib where

import           Control.Applicative
import           Data.Kind
import           Data.Maybe
import           Data.Proxy
import           Data.Time
import           GHC.TypeLits
import           Text.Read


-- FAKE WAI APPLICATION
type Application = [String] -> IO String


data Get (a :: Type)

data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: Type)
infixr 9 :>

data Capture (a :: Type)


type family Server layout :: Type

type instance Server (Get a) = IO a
type instance Server (a :<|> b) = Server a :<|> Server b
type instance Server ((s :: Symbol) :> rest) = Server rest
type instance Server (Capture a :> rest) = a -> Server rest


class HasServer layout where
  route :: Proxy layout -> Server layout -> [String] -> Maybe (IO String)

instance Show a => HasServer (Get a) where
  route :: Proxy (Get a) -> IO a -> [String] -> Maybe (IO String)
  route _ handler [] = Just (fmap show handler)
  route _ _       _  = Nothing

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route :: Proxy (a :<|> b) -> (Server a :<|> Server b) -> [String] -> Maybe (IO String)
  route _ (handlerA :<|> handlerB) xs =
    route (Proxy :: Proxy a) handlerA xs
    <|> route (Proxy :: Proxy b) handlerB xs

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  route :: Proxy (s :> r) -> Server r -> [String] -> Maybe (IO String)
  route _ handler (x : xs) | symbolVal (Proxy :: Proxy s) == x =
    route (Proxy :: Proxy r) handler xs
  route _ _ _ = Nothing

instance (Read a, HasServer r) => HasServer (Capture a :> r) where
  route :: Proxy (Capture a :> r) -> (a -> Server r) -> [String] -> Maybe (IO String)
  route _ handler (x : xs) = do
    a <- readMaybe x
    route (Proxy :: Proxy r) (handler a) xs
  route _ _ _ = Nothing

serve :: HasServer layout => Proxy layout -> Server layout -> Application
serve proxy server request =
  fromMaybe (ioError (userError "404")) (route proxy server request)



type MyApi = 
  "date" :> Get Day
  :<|> "time" :> Capture TimeZone :> Get ZonedTime

handleDate :: IO Day
handleDate = fmap utctDay getCurrentTime

handleTime :: TimeZone -> IO ZonedTime
handleTime timeZone = utcToZonedTime timeZone <$> getCurrentTime

myApiServer :: Server MyApi
myApiServer = handleDate :<|> handleTime

myApiApplication :: Application
myApiApplication = serve (Proxy :: Proxy MyApi) myApiServer
