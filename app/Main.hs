{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Concurrent.MVar

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import System.Random
import Control.Monad.IO.Class (liftIO)

import Servant
import Network.Wai.Handler.Warp
import qualified Network.URI.Encode as URIEncode

import Web.Mastodon.Lib.Registration


mastodonAuthEndpoint :: T.Text
mastodonAuthEndpoint = "/oauth/authorize"

data LmrsState = LmrsState
    { urlMap :: MVar (HM.Map AuthCode (AppInfo,Maybe T.Text))
    , serverLocation :: T.Text
    }

lmrsServer :: LmrsState -> Server LmrsAPI
lmrsServer s = startAuth
         :<|> authBegun s
         :<|> auth s
         :<|> authComplete s
         :<|> authResult s

startAuth :: Handler AuthCode
startAuth = do
    g <- liftIO $ getStdGen
    let code = take 8 $ randomRs ('a','z') g
    return $ T.pack code

authBegun :: LmrsState -> AuthCode -> AppInfo -> Handler ()
authBegun s c a = do
    hmap <- liftIO $ takeMVar (urlMap s)
    let hmap' = HM.insert c (a,Nothing) hmap
    liftIO $ putMVar (urlMap s) hmap'
    return ()
    
auth :: LmrsState -> AuthCode -> Handler ()
auth s c = do
    hmap <- liftIO $ readMVar (urlMap s)
    let mAppInfo = HM.lookup c hmap
    case mAppInfo of
        Just (a,_) -> throwError $ err303
                        { errHeaders = [("Location", genRedirect s a c)] }
        Nothing    -> throwError err404
  where
    f :: T.Text -> T.Text -> T.Text
    f a b = a `T.append` "=" `T.append` b

    genSuccessURI :: LmrsState -> AuthCode -> T.Text
    genSuccessURI s' c' =
        serverLocation s' `T.append` "/authcomplete/" `T.append` c'

    genRedirect :: LmrsState -> AppInfo -> AuthCode -> BS.ByteString
    genRedirect s' a' c' =
        let server'        = server a'
            response_type' = "code"
            client_id'     = client_id a'
            client_secret' = client_secret a'
            redirect_uri'  = URIEncode.encodeText $ genSuccessURI s' c'
            scope'         = URIEncode.encodeText "read write follow"
        in E.encodeUtf8 $
            server' `T.append` mastodonAuthEndpoint
                `T.append` "?" `T.append` f "response_type" response_type'
                `T.append` "&" `T.append` f "client_id"     client_id'
                `T.append` "&" `T.append` f "client_secret" client_secret'
                `T.append` "&" `T.append` f "redirect_uri"  redirect_uri'
                `T.append` "&" `T.append` f "scope"         scope'

authComplete :: LmrsState -> AuthCode -> Maybe T.Text -> Handler AuthResult
authComplete s c moauthcode = do
    hmap <- liftIO $ takeMVar (urlMap s)
    case (moauthcode,HM.lookup c hmap) of
        (Just oauthcode,Just (a,_)) -> do
            liftIO $ putMVar (urlMap s) (HM.insert c (a,Just oauthcode) hmap)
            return (AuthSuccess a)
        _ -> do
            liftIO $ putMVar (urlMap s) hmap
            throwError err404

authResult :: LmrsState -> AuthCode -> Handler OAuthCode
authResult s c = do
    hmap <- liftIO $ readMVar (urlMap s)
    case HM.lookup c hmap of
        Just (_,Just code) -> return $ OAuthCode code
        _                  -> throwError err404


app :: LmrsState -> Application
app s = serve lmrsAPI (lmrsServer s)

main :: IO ()
main = do
    hmap <- newMVar (HM.empty)
    let s = LmrsState hmap "https://smuggle.rs"
    run 8080 (app s)
