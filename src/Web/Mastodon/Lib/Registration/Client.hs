{-# LANGUAGE TypeOperators #-}
module Web.Mastodon.Lib.Registration.Client
    ( startAuth
    , authBegun
    , authResults
    , authCodeToURL
    )
  where

import Data.Aeson
import qualified Data.Text as T

import Servant
import Servant.Client

import Web.Mastodon.Lib.Registration

startAuth :: ClientM AuthCode
authBegun :: AuthCode -> AppInfo -> ClientM ()
auth :: AuthCode -> ClientM ()
authComplete :: AuthCode -> Maybe T.Text -> ClientM AuthResult
authResults :: AuthCode -> ClientM OAuthCode

startAuth
  :<|> authBegun
  :<|> auth
  :<|> authComplete
  :<|> authResults
    = client lmrsAPI

authCodeToURL :: T.Text -> AuthCode -> T.Text
authCodeToURL server authCode =
        server `T.append` "/auth/" `T.append` authCode
