# yesod-recaptcha2

![Hackage](https://img.shields.io/hackage/v/yesod-recaptcha2.svg)

~~~hs
import Yesod.ReCaptcha2
~~~

~~~hs
instance YesodReCaptcha App where
    reCaptchaSiteKey = pure "foo"
    reCaptchaSecretKey = pure "bar"
    reCaptchaLanguage = pure Nothing

    -- with specific language from https://developers.google.com/recaptcha/docs/language
    -- reCaptchaLanguage = pure (Just "ru")
~~~

## Append to applicative form

~~~hs
buildForm :: Form MyForm
buildForm = renderDivs $ MyForm
  <$> areq textField "foo" Nothing
  <* reCaptcha
~~~
