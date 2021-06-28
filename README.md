# yesod-recaptcha2

[![Hackage](https://img.shields.io/hackage/v/yesod-recaptcha2.svg)](https://hackage.haskell.org/package/yesod-recaptcha2)
[![Build Status](https://travis-ci.org/ncaq/yesod-recaptcha2.svg?branch=master)](https://travis-ci.org/ncaq/yesod-recaptcha2)

~~~hs
import Yesod.ReCaptcha2
~~~

~~~hs
instance YesodReCaptcha App where
    reCaptchaSiteKey = pure "foo"
    reCaptchaSecretKey = pure "bar"
    reCaptchaLanguage = pure Nothing
~~~

## Append to applicative form

~~~hs
buildForm :: Form MyForm
buildForm = renderDivs $ MyForm
  <$> areq textField "foo" Nothing
  <* reCaptcha
~~~
