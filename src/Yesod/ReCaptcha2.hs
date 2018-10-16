{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Yesod.ReCaptcha2 (YesodReCaptcha(..), reCaptcha, mReCaptcha) where

import           ClassyPrelude
import           Data.Aeson
import           Network.HTTP.Simple
import           Yesod.Auth
import           Yesod.Core.Handler
import           Yesod.Core.Types
import           Yesod.Core.Widget
import           Yesod.Form.Functions
import           Yesod.Form.Types

-- | default key is testing. you should impl reCaptchaSiteKey and reCaptchaSecretKey
class YesodAuth site => YesodReCaptcha site where
    reCaptchaSiteKey :: HandlerFor site Text
    reCaptchaSiteKey = pure "6LeIxAcTAAAAAJcZVRqyHh71UMIEGNQ_MXjiZKhI"
    reCaptchaSecretKey :: HandlerFor site Text
    reCaptchaSecretKey = pure "6LeIxAcTAAAAAGG-vFI1TnRWxMZNFuojJ4WifJWe"
    -- | with specific language from <https://developers.google.com/recaptcha/docs/language>
    --
    -- > reCaptchaLanguage = pure (Just "ru")
    reCaptchaLanguage :: HandlerFor site (Maybe Text)
    reCaptchaLanguage = pure Nothing

newtype SiteverifyResponse
    = SiteverifyResponse
    { success :: Bool
    }
    deriving (Eq, Ord, Read, Show, Generic, FromJSON, ToJSON)

-- | for Applicative style form
reCaptcha :: YesodReCaptcha site => AForm (HandlerFor site) ()
reCaptcha = formToAForm mReCaptcha

-- | for Monadic style form
mReCaptcha :: YesodReCaptcha site => MForm (HandlerFor site) (FormResult (), [FieldView site])
mReCaptcha = do
    result <- lift formResult
    return (result, [fieldViewSite])
  where formResult = do
            postParam <- lookupPostParam "g-recaptcha-response"
            case postParam of
                Nothing -> return FormMissing
                Just response -> do
                    secret <- reCaptchaSecretKey
                    SiteverifyResponse{success} <- liftIO $ do
                        req <- parseRequest "POST https://www.google.com/recaptcha/api/siteverify"
                        res <- httpJSON $
                            setRequestBodyURLEncoded
                            [("secret", encodeUtf8 secret), ("response", encodeUtf8 response)] req
                        return $ getResponseBody res
                    return $ if success
                        then FormSuccess ()
                        else FormFailure ["reCaptcha error"]
        fieldViewSite = FieldView
            { fvLabel = mempty
            , fvTooltip = Nothing
            , fvId = ""
            , fvInput = do
                    mReCaptchaLanguage <- handlerToWidget reCaptchaLanguage
                    case mReCaptchaLanguage of
                      Nothing ->
                        addScriptRemote "https://www.google.com/recaptcha/api.js"
                      Just hl ->
                        addScriptRemote $ "https://www.google.com/recaptcha/api.js?hl=" <> hl
                    siteKey <- handlerToWidget reCaptchaSiteKey
                    [whamlet|<div .g-recaptcha data-sitekey=#{siteKey}>|]
            , fvErrors = Nothing
            , fvRequired = True
            }
