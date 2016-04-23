Route and handlers
------------------

Again, you can run this "site" by typing:

`stack exec site2`

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

And WAI and Warp are the same:

> main :: IO ()
> main = run 3000 waiApp

> waiApp :: Application
> waiApp = toWAI (Context defaultFnRequest) site

But `site` is going to be a bit different, because we're going to add more routes!

> site :: Context -> IO Response
> site ctxt = route ctxt [ end ==> indexHandler
>                        , path "hello" // end ==> helloHandler
>                        , path "hello" // segment // end ==> helloNameHandler
>                        , path "add" // end ==> addHandler
>                        , path "add" // segment // segment // end ==> addNumbersHandler
>                        , path "add" // segment // segment // end ==> addWordsHandler
>                        ]
>             `fallthrough` notFoundText "Page not found."

`route` is a function that takes a RequestContext and a list of
routes. It will try to match the current requested path against one of
the paths listed in the routes. Fn lets you use the `//` operator to
combine different url patterns into a complete path that might match a
request. Then you use the `==>` operator to tell which Handler might
handle a request for that path.

I say "might" match or handle a request because there's always the possibility
that while, say, "/add/2/2" makes sense for the "addNumbersHandler" to handle,
maybe "/add/apple/oranges" should be handled by the "addWordsHandler". But if
none of the routes can handle it, the request will "fallthrough" and `site`
will respond with a the text "Page not found."

> indexHandler :: Context -> IO (Maybe Response)
> indexHandler ctxt = okText "Welcome to my SECOND Haskell website! Try visiting \"hello\" or \"add\"!"

Hey, it's our old friend the indexHandler! And a very similar one
called `helloHandler`. This time, let's take a closer look at the type
signature. It takes a Context and returns something with this other
type, `IO (Maybe Response)`.

[`okText`](http://hackage.haskell.org/package/fn-0.3.0.1/docs/Web-Fn.html#v:okText)
has the type `Text -> IO (Maybe Response)`. We give it Text, it
creates a Response with the right status code (200) and content type
(text/plain), then returns "Just" that response.

Here's a the handler which will handle "localhost:3000/hello", according to this route: `path "hello" // end ==> helloHandler`.
This handler looks very similar!

> helloHandler :: Context -> IO (Maybe Response)
> helloHandler ctxt = okText "Hello, world!"

But the next route is a little different. `path "hello" // segment //
end ==> helloNameHandler` can handle a URL like
"localhost:3000/hello/libby".  The `segment` pattern means that part of
the URL is passed as an argument to the handler. This also affects
the type of the handler.

> helloNameHandler :: Context -> Text -> IO (Maybe Response)
> helloNameHandler ctxt name = okText ("Hello, " <> name <> "!")

This Handler has an additional argument and we've specified that it's
text so that we can combine it with the word "hello" to create a
greeting.

So far all these Handlers always return `okText`, which I said earlier
is "Just" some response. Can the handlers return Nothing instead? They
sure can!

> silentTreatmentHandler :: Context -> Text -> IO (Maybe Response)
> silentTreatmentHandler ctxt name =
>   case name of
>   "dbp"     -> return Nothing
>   otherwise -> okText ("Hello, " <> name <> "!")

This is a handler that can choose to ignore some people! If I use this
handler instead of `helloNameHandler`, then visit
"localhost:3000/hello/Libby", I still get "Hello, Libby!". But if I
type "localhost:3000/hello/dbp", the handler chooses NOT to handle
that request. Poor dbp gets ignored. :(

Exercises
---------

Add `silentTreatmentHandler` as a route to site. Is it possible to
have both `helloNameHandler` and `silentTreatmentHandler`? Does it
do what you would expect?

Add a `rudeHello` that "catches" requests that fall through
`silentTreatmentHandler`.

Matching on types
-----------------

> addHandler :: Context -> IO (Maybe Response)
> addHandler ctxt = okText "Find the result of adding different things by visitng \"add/firstThing/secondThing/\""

> addNumbersHandler :: Context -> Int -> Int -> IO (Maybe Response)
> addNumbersHandler ctxt firstNumber secondNumber =
>   okText $ "The sum of " <> tShow firstNumber <> " and " <> tShow secondNumber
>            <> " is " <> tShow (firstNumber + secondNumber)

> addWordsHandler :: Context -> Text -> Text -> IO (Maybe Response)
> addWordsHandler ctxt firstWord secondWord =
>   okText $ firstWord <> " and " <> secondWord <> " added together is "
>            <> (firstWord <> secondWord)

[Back to previous](http://fnhaskell.com/tutorial/Site1.html)

[To the next](http://fnhaskell.com/tutorial/Site3.html)
