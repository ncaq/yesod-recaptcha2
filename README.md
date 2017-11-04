# yesod-recaptcha2

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


~~~hs
<* reCaptcha
~~~

Append to applicative form:

~~~hs
buildForm :: Form MyForm
buildForm = renderDivs $ MyForm
  <$> areq textField myFieldSettings Nothing
  <* reCaptcha
  where
    myFieldSettings = ...
~~~
