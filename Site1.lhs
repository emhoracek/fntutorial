Fn Tutorial

Fn is a simple web framework build in Haskell.

This tutorial will show you how to make a very simple website with Fn.

The Simplest Site
-----------------

stack build
stack exec site1

visit localhost:3000

How does it work?!

well --- this is the source!! This page is written in "Literate Haskell", so
you can run it as source code. I'll take you line by line and show you what's
going on.

> {-# LANGUAGE OverloadedStrings #-}

> import Web.Fn
> import Network.Wai (Response, Application)
> import Network.Wai.Handler.Warp (run)
> import Data.Text (Text)

These import the libraries we'll need for the website: Fn, WAI (Web Application
Interface), and Warp. In Haskell, you can import modules from libraries with
`import` + the module name. Then you can optionally restrict the imports to just
certain things. So we're only importing "Response" and "Application"
from Wai and only "run" from Warp.

Why are we importing these specific things?

* Fn is our web framework
* Warp is the server
* WAI is middleware (like it sounds, stuff in the middle between the framework and server).

> data Context = Context { req :: FnRequest }

Next we make the context for our application. The idea is that
there's data we want to access when we're preparing a Response to send out to
a user. That data might be in a database, a cache, environment variables, wherever! In
Fn, we wrap all those things up in a "Context". Since in this part of the tutorial, we're
making the simplest app possible, we're only going to worry about the most
basic information the user sends us -- the request.

> instance RequestContext Context where
>  getRequest ctxt = req ctxt
>  setRequest ctxt newRequest = ctxt { req = newRequest }

These lines make `Context` an "instance" of `RequestContext`. Fn will use these
functions, `getRequest` and `setRequest`, to help us respond to requests.

> data PizzaJungle = Totoro { request :: FnRequest,
>                             mothersMaidenName :: String,
>                             isAmbidextrous :: Bool }

> instance RequestContext PizzaJungle where
>   getRequest pizzaJungle = request pizzaJungle
>   setRequest pizzaJungle newReq = pizzaJungle { request = newReq }

Okay, I got very very silly here, but I want to make the point that it's not
really important that your app specifically have a `Context`, but it's important that
it has *something* (even a `PizzaJungle`!) that's a `RequestContext`. Also
notice that you can as many and whatever fields you want in your context, as
long as you also have some field (name doesn't matter!) that's an `FnRequest`.</span>

Exercise
--------

Suppose your app had the `AppContext` below.

> data AppContext = AppContext { fnrequest :: FnRequest,
>                                secretKey :: Text }

How could you write the `RequestContext` instance for `AppContext`?

> -- uncomment the next lines by deleting the "--"s when you have a solution!
> -- instance RequestContext AppContext where
> --   getRequest ????
> --   setRequest ????

(Need help?)

--------

Okay, so back to `Context`. How do we actually use that?

> site :: Context -> IO Response
> site ctxt = route ctxt [ end ==> indexHandler ]
>                  `fallthrough` notFoundText "Page not found."

Our "site" is a way to take our Context, and turn it into a
Response. Don't worry too much about it right now because I'll go into much
more depth in the next part on routes!

> indexHandler :: Context -> IO (Maybe Response)
> indexHandler ctxt = okText "Welcome to my first Haskell website."

Hey, it's our first handler! This handles a request by taking a context and
giving back a response (again, we'll go into more detail on just what that
`IO (Maybe Response)` there means in the next part). In this case, we're going
to do the same thing every time someone requests this page -- just send a message.

> main :: IO ()
> main = run 3000 waiApp

> waiApp :: Application
> waiApp = toWAI (Context defaultFnRequest) site

I said earlier that WAI is the middleware between our site and the server. This
is where we use WAI. Warp expects an app, so we use Fn's "toWAI" function to
turn an initial context and our `site` into an Application. `run 3000 waiApp`
means to run the Warp server with this app on port 3000.

Exercises
---------

Change the site and handler to use the AppContext data structure instead of
Context. (You'll need to tell `waiApp` how to build an AppContext:
`AppContext defaultFnRequest "your string here"`). Can you make the
`indexHandler` return a response containing the `secretKey`?
