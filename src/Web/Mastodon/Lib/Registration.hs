{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Web.Mastodon.Lib.Registration where

import Data.Aeson
import qualified Data.Text as T

import GHC.Generics

import Servant
import Servant.HTML.Lucid
import Lucid.Base
import Lucid.Html5

data AppInfo = AppInfo
    { app_id        :: Int
    , client_id     :: T.Text
    , client_secret :: T.Text
    , server        :: T.Text
    , app_name      :: T.Text
    } deriving(Show, Generic)

instance ToJSON   AppInfo
instance FromJSON AppInfo

data OAuthCode = OAuthCode { oauth_code :: T.Text } deriving(Show, Generic)
instance ToJSON   OAuthCode
instance FromJSON OAuthCode

data AuthResult = AuthSuccess AppInfo
instance ToHtml AuthResult where
  toHtml (AuthSuccess a) =
    html_ $ do
        h1_ "Auth Successful"
        p_ $ toHtml ("You have successfully logged in with " ++ T.unpack (server a))
        p_ $ toHtml ("Return to " ++ T.unpack (app_name a) ++ " to proceed")
  toHtmlRaw = toHtml
instance MimeUnrender HTML AuthResult where
    mimeUnrender _ _ = Left "unsupported"

type AuthCode = T.Text

type LmrsAPI =
         "startauth"
      :> Get '[JSON] AuthCode

    :<|> "authbegun"
      :> Capture "authid" AuthCode
      :> ReqBody '[JSON] AppInfo
      :> Post '[JSON] ()

    :<|> "auth"
      :> Capture "authid" AuthCode
      :> Get '[JSON] ()

    :<|> "authcomplete"
      :> Capture "authid" AuthCode
      :> QueryParam "code" T.Text
      :> Get '[HTML] AuthResult

    :<|> "authresults"
      :> Capture "authid" AuthCode
      :> Get '[JSON] OAuthCode

lmrsAPI :: Proxy LmrsAPI
lmrsAPI = Proxy
