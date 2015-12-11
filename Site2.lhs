Fn Tutorial, part 2!

Route and handlers!

Again you can run this "site" by typing:

stack exec site2

And again, I'll take you through how it works, line by line!

First, lets get the basics from part 1 out of the way:

> {-# LANGUAGE OverloadedStrings #-}
> 
> import Web.Fn
> import Network.Wai (Response, Application)
> import Network.Wai.Handler.Warp (run)
> 

And we'll need a few more imports:

> import qualified Data.Text as T
> import Data.Text (Text)

The first is a library for using Text. Haskell has a string type, but Text is
better in (link) lots of ways. We're importing it twice -- the first time we
import the whole library "qualified". That means we have to prefix every
function or type we use from that module with "T.", so `T.toUpper` instead of
just `toUpper`. Yuck, that's kinda annoying! So why? It's because this Data.Text
has some functions with the same name as more common functions. For example,
`reverse` in Haskell's standard library reverses a list, but Data.Text has a
`reverse` that reverses Text. Importing it "qualified" tells the compiler
which "reverse" we mean.

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
>                        , path "add" // segment // end ==> addWhatHandler
>                        , path "add" // segment // segment ==> addNumbersHandler
>                        , path "add" // segment // segment ==> addWordsHandler
>                        ]
>             `fallthrough` notFoundText "Page not found."
             
`route` is a function that takes a RequestContext and a list of routes. It will
try to match the current requested path against one of the paths listed in the
routes. Fn lets you use the `//` operator to combine different types of
segments into a complete path that might match a request. Then you use the
`==>` operator to tell which Handler might handle a request for that path.

I say "might" match or handle a request because there's always the possibility 
that while, say, "/add/2/2" makes sense for the "addNumbersHandler" to handle,
maybe "/add/apple/oranges" should be handled by the "addFruitHandler". But if
none of the routes can handle it, the request will "fallthrough" and `site`
will respond with a the text "Page not found."

Have you noticed I'm saying "Maybe" a lot?

> indexHandler :: Context -> IO (Maybe Response)
> indexHandler ctxt = okText "Welcome to my SECOND Haskell website! Try visiting \"add\"!"
>
> addHandler :: Context -> IO (Maybe Response)
> addHandler ctxt = okText "Find the result of adding different things by visitng \"add/firstThing/secondThing/\""

Hey it's our old friend the indexHandler! And a very similar one called `addHandler`. This time lets take a closer look at
that first line. That is called a "type signature". You could read that line
aloud as "indexHandler has the type Context to IO of Maybe Response". It takes
a Context and returns somethign with this other type, `IO (Maybe Response)`.

IO? of Maybe Response? What? Those are still just types. IO is a special sort of
type that indicates some action, like getting a data from a database or reading
a file. It's really useful to be able to do those things in a handler, so
Handler functions have the type IO. But we said the IO is an action -- an
action that does something. Our type doesn't say what that action might be,
but it does say that it has to return a value with a certain other type --
`Maybe Response`.

> addWhatHandler :: Context -> Text -> IO (Maybe Response)
> addWhatHandler ctxt t =
>   Just <$> notFoundText ("What should I add to " `T.append` t `T.append` "?")

It doesn't make sense to "add" just one thing. You need something to add it to.
So this is notFound.

> addNumbersHandler :: Context -> Int -> Int -> IO (Maybe Response)
> addNumbersHandler ctxt firstNumber secondNumber =
>   okText $ T.pack $ show (firstNumber + secondNumber)

> addWordsHandler :: Context -> Text -> Text -> IO (Maybe Response)
> addWordsHandler ctxt firstWord secondWord =
>   okText (firstWord `T.append` secondWord)


> main :: IO ()
> main = run 3000 waiApp
>
> waiApp :: Application
> waiApp = toWAI (Context defaultFnRequest) site