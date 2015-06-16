module Googl
  ( ShortURL(..)
  , shortenURL
  , shorten ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder
import Prelude

newtype APIKey = APIKey Text
  deriving (Show, Read, Eq)

data ShortURL =
  ShortURL { urlID :: Text
           , longURL :: Text}
  deriving (Show, Read, Eq)

instance FromJSON ShortURL where
  parseJSON (Object o) = do
     ensureKind o "urlshortener#url"
     ShortURL <$> o .: "id"
              <*> o .: "longUrl"
  parseJSON _ = mempty

instance Receivable ShortURL where
  receive = useFromJSON

ensureKind :: Object -> Text -> Parser ()
ensureKind o k = do
  kind <- o .: "kind"
  guard $ kind == k

googl :: Builder
googl = basicBuilder "Goo.gl Link Shortener" "https://www.googleapis.com/urlshortener"

shortenURL :: Text -> IO (Either (APIError ()) ShortURL)
shortenURL t = execAPI googl () $
  sendRoute (object ["longUrl" .= t]) route
  where
    route = Route ["v1", "url"]
                  [ ]
                  "POST"

shorten :: Text -> IO (Maybe ShortURL)
shorten t = do
  res <- shortenURL t
  case res of
    Left _ -> return Nothing
    Right x -> return (Just x)
