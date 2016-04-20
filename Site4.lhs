Fn Tutorial, part 4!

Serving static files.

Again you can run this "site" by typing:

stack exec site3

And again, I'll take you through how it works, line by line!

Getting the basics out of the way:

> {-# LANGUAGE OverloadedStrings #-}

> import Web.Fn
> import Network.Wai (Response, Application)
> import Network.Wai.Handler.Warp (run)
> import Data.Monoid ((<>))
> import qualified Data.Text as T
> import Data.Text (Text)
> import Data.Text.Lazy (toStrict)
> import Lucid

Still need these text and Lucid helpers, and the same Context:

> tShow :: Show a => a -> Text
> tShow = T.pack . show

> lucidHtml :: Html () -> IO (Maybe Response)
> lucidHtml h = okHtml $ toStrict $ renderText h

> data Context = Context { req :: FnRequest }
> instance RequestContext Context where
>  getRequest ctxt = req ctxt
>  setRequest ctxt newRequest = ctxt { req = newRequest }

But next we'll allow static serving with a handler from Fn:

> site :: Context -> IO Response
> site ctxt = route ctxt [ end ==> indexHandler
>                        , path "add" // param "t1" // param "t2" ==> addNumbersHandler
>                        , path "add" // param "t1" // param "t2" ==> addWordsHandler
>                        , path "add" // end ==> addHandler
>                        , anything ==> staticServe "static"
>                        ]
>             `fallthrough` notFoundText "Page not found."

We're doing that to add stylesheets, so lets make a "template" for the Lucid HTML.

> baseView :: Text -> Html () -> Html ()
> baseView title rest = do
>   html_ $ do
>     head_ $ do
>       link_ [href_ "style.css", rel_ "stylesheet", type_ "text/css"]
>       title_ (toHtml title)
>     body_ $ rest

Then we can write all our views using the same base.

> indexView :: Html ()
> indexView =
>   baseView "My fourth Haskell site" $ do
>       h1_ "My fourth Haskell site"
>       p_ "Try visiting \"add\"!"

You can functions in a do block the same way you'd nest tags! This is pretty cool.

But look at the type! `indexView` has the type `Html ()`. Our handlers
return `Maybe Response`.

Fn provides `okHtml`, a "200 OK" response for HTML, which expects
`Text`. So we have fill the gap between Lucid's `Html ()` and `Text`.

Lucid's `renderText` takes `Html ()` and returns lazy text. Then we
can use `toStrict` to turn it into the `Text` we want.

> indexHandler :: Context -> IO (Maybe Response)
> indexHandler ctxt = lucidHtml indexView

So beautiful!

Let's use HTML to create a form for the `addHandler` as well:

> addView :: Html ()
> addView =
>   baseView "Addertron" $ do
>       form_ [action_ "add"] $ do
>         label_ [for_ "t1"] "Thing 1:"
>         input_ [id_ "t1", name_ "t1", type_ "text"]
>         label_ [for_ "t2"] "Thing 2:"
>         input_ [id_ "t2", name_ "t2", type_ "text"]
>         input_ [type_ "submit"]

> addHandler :: Context -> IO (Maybe Response)
> addHandler ctxt = lucidHtml addView

Let's inline some HTML into these handlers:

> addNumbersHandler :: Context -> Int -> Int -> IO (Maybe Response)
> addNumbersHandler ctxt firstNumber secondNumber =
>   lucidHtml $ baseView "Addertron results" $ do
>     p_ $ toHtml ("The sum of " <> tShow firstNumber <> " and " <> tShow secondNumber
>                  <> " is " <> tShow (firstNumber + secondNumber))

> addWordsHandler :: Context -> Text -> Text -> IO (Maybe Response)
> addWordsHandler ctxt firstWord secondWord =
>   lucidHtml $ baseView "Addertron results" $ do
>     p_ $ toHtml (firstWord <> " and " <> secondWord <> " added together is "
>                  <> (firstWord <> secondWord))

> main :: IO ()
> main = run 3000 waiApp

> waiApp :: Application
> waiApp = toWAI (Context defaultFnRequest) site
