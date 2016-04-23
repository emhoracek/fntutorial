Fn Tutorial, part 3!

Using Lucid for HTML responses.

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

For this site, we're going to add HTML with Lucid!

> import Lucid

Still need this text helper:

> tShow :: Show a => a -> Text
> tShow = T.pack . show

We'll use the same Context:

> data Context = Context { req :: FnRequest }
> instance RequestContext Context where
>  getRequest ctxt = req ctxt
>  setRequest ctxt newRequest = ctxt { req = newRequest }

Because we still don't need anything besides a request from a user.

But `site` is going to be a bit different.

> site :: Context -> IO Response
> site ctxt = route ctxt [ end ==> indexHandler
>                        , path "add" // param "t1" // param "t2" ==> addNumbersHandler
>                        , path "add" // param "t1" // param "t2" ==> addWordsHandler
>                        , path "add" // end ==> addHandler
>                        ]
>             `fallthrough` notFoundText "Page not found."

The routes have changed a little bit. Now we're matching on parameters
instead of path segments. (Had to move addHandler to bottom).

Our handlers are going to change because now we're going to use Lucid
to create HTML templates.

Here's what building up a Lucid template might look like:

> indexView :: Html ()
> indexView = do
>   html_ $ do
>     head_ $ do
>       title_ "My third Haskell site"
>     body_ $ do
>       h1_ "My third Haskell site"
>       p_ "Try visiting \"add\"!"

You can put functions in a do block the same way you'd nest tags! This is pretty cool.

But look at the type! `indexView` has the type `Html ()`. Our handlers
return `Maybe Response`.

Fn provides `okHtml`, a "200 OK" response for HTML, which expects
`Text`. So we have fill the gap between Lucid's `Html ()` and `Text`.

> lucidHtml :: Html () -> IO (Maybe Response)
> lucidHtml h = okHtml $ toStrict $ renderText h

Lucid's `renderText` takes `Html ()` and returns lazy text. Then we
can use `toStrict` to turn it into the `Text` we want.

> indexHandler :: Context -> IO (Maybe Response)
> indexHandler ctxt = lucidHtml indexView

So beautiful!

Let's use HTML to create a form for the `addHandler` as well:

> addView :: Html ()
> addView = do
>   html_ $ do
>     body_ $ do
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
>   lucidHtml $ do
>     p_ $ toHtml ("The sum of " <> tShow firstNumber <> " and " <> tShow secondNumber
>                  <> " is " <> tShow (firstNumber + secondNumber))

> addWordsHandler :: Context -> Text -> Text -> IO (Maybe Response)
> addWordsHandler ctxt firstWord secondWord =
>   lucidHtml $ do
>     p_ $ toHtml (firstWord <> " and " <> secondWord <> " added together is "
>                  <> (firstWord <> secondWord))

> main :: IO ()
> main = run 3000 waiApp

> waiApp :: Application
> waiApp = toWAI (Context defaultFnRequest) site

[Back to previous](http://fnhaskell.com/tutorial/Site2.html)

[To the next](http://fnhaskell.com/tutorial/Site4.html)
