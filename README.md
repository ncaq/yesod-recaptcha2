# yesod-recaptcha2

~~~hs
import Import.ReCaptcha2
~~~

~~~hs
instance YesodReCaptcha App where
    reCaptchaSiteKey = return "foo"
    reCaptchaSecretKey = return "bar"
~~~


~~~hs
<* reCaptcha
~~~

