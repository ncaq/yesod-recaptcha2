{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Yesod.ReCaptcha2 (YesodReCaptcha(..), reCaptcha, mReCaptcha) where

import           ClassyPrelude.Yesod
import           Network.HTTP.Simple
import           Yesod.Auth
import           Data.Text (append)

-- | default key is testing. you should impl reCaptchaSiteKey and reCaptchaSecretKey
class YesodAuth site => YesodReCaptcha site where
    reCaptchaSiteKey :: HandlerT site IO Text
    reCaptchaSiteKey = pure "6LeIxAcTAAAAAJcZVRqyHh71UMIEGNQ_MXjiZKhI"
    reCaptchaSecretKey :: HandlerT site IO Text
    reCaptchaSecretKey = pure "6LeIxAcTAAAAAGG-vFI1TnRWxMZNFuojJ4WifJWe"
    reCaptchaLanguage :: HandlerT site IO (Maybe Text)
    reCaptchaLanguage = pure Nothing

data SiteverifyResponse = SiteverifyResponse { success :: Bool }
    deriving (Eq, Ord, Read, Show, Generic, FromJSON, ToJSON)

-- | for Applicative style form
reCaptcha :: YesodReCaptcha site => AForm (HandlerT site IO) ()
reCaptcha = formToAForm mReCaptcha

-- | for Monadic style form
mReCaptcha :: YesodReCaptcha site => MForm (HandlerT site IO) (FormResult (), [FieldView site])
mReCaptcha = do
    result <- liftHandlerT formResult
    return (result, [fieldViewSite])
  where formResult = do
            postParam <- lookupPostParam "g-recaptcha-response"
            case postParam of
                Nothing -> return $ FormMissing
                Just response -> do
                    secret <- reCaptchaSecretKey
                    s@SiteverifyResponse { success } <- liftIO $ do
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
                    language <- handlerToWidget reCaptchaLanguage
                    case language of
                      Nothing ->
                        addScriptRemote "https://www.google.com/recaptcha/api.js"
                      Just language ->
                        addScriptRemote $ append "https://www.google.com/recaptcha/api.js?hl=" language
                    siteKey <- handlerToWidget reCaptchaSiteKey
                    [whamlet|<div .g-recaptcha data-sitekey=#{siteKey}>|]
            , fvErrors = Nothing
            , fvRequired = True
            }
