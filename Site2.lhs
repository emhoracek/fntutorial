Fn Tutorial, part 2!

Route and handlers!

Again you can run this "site" by typing:

stack exec site2

And again, I'll take you through how it works, line by line!

First, lets get the basics from part 1 out of the way:

> {-# LANGUAGE OverloadedStrings #-}

> import Web.Fn
> import Network.Wai (Response, Application)
> import Network.Wai.Handler.Warp (run)

And some useful stuff for text:

> import Data.Monoid ((<>))
> import qualified Data.Text as T
> import Data.Text (Text)

> tShow :: Show a => a -> Text
> tShow = T.pack . show

We'll use the same Context:

> data Context = Context { req :: FnRequest }
> instance RequestContext Context where
>  getRequest ctxt = req ctxt
>  setRequest ctxt newRequest = ctxt { req = newRequest }

Because we still don't need anything besides a request from a user.

But `site` is going to be a bit different, because we're going to add more routes!

> site :: Context -> IO Response
> site ctxt = route ctxt [ end ==> indexHandler
>                        , path "add" // end ==> addHandler
>                        , path "add" // segment // segment // end ==> addNumbersHandler
>                        , path "add" // segment // segment // end ==> addWordsHandler
>                        ]
>             `fallthrough` notFoundText "Page not found."

`route` is a function that takes a RequestContext and a list of routes. It will
try to match the current requested path against one of the paths listed in the
routes. Fn lets you use the `//` operator to combine different types of
segments into a complete path that might match a request. Then you use the
`==>` operator to tell which Handler might handle a request for that path.

I say "might" match or handle a request because there's always the possibility
that while, say, "/add/2/2" makes sense for the "addNumbersHandler" to handle,
maybe "/add/apple/oranges" should be handled by the "addWordsHandler". But if
none of the routes can handle it, the request will "fallthrough" and `site`
will respond with a the text "Page not found."

> indexHandler :: Context -> IO (Maybe Response)
> indexHandler ctxt = okText "Welcome to my SECOND Haskell website! Try visiting \"add\"!"

> addHandler :: Context -> IO (Maybe Response)
> addHandler ctxt = okText "Find the result of adding different things by visitng \"add/firstThing/secondThing/\""

Hey it's our old friend the indexHandler! And a very similar one
called `addHandler`. This time lets take a closer look at the type
signature. It takes a Context and returns somethign with this other
type, `IO (Maybe Response)`.

> addNumbersHandler :: Context -> Int -> Int -> IO (Maybe Response)
> addNumbersHandler ctxt firstNumber secondNumber =
>   okText $ "The sum of " <> tShow firstNumber <> " and " <> tShow secondNumber
>            <> " is " <> tShow (firstNumber + secondNumber)

> addWordsHandler :: Context -> Text -> Text -> IO (Maybe Response)
> addWordsHandler ctxt firstWord secondWord =
>   okText $ firstWord <> " and " <> secondWord <> " added together is "
>            <> (firstWord <> secondWord)

> main :: IO ()
> main = run 3000 waiApp

> waiApp :: Application
> waiApp = toWAI (Context defaultFnRequest) site
