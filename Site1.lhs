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

Haskell is a pretty small language without a lot of baked-in features. You can
add extra features by including"pragmas" like this to beginning of the file.
"OverloadedStrings" makes working with text a lot easier than it is in plain
Haskell.

> import Web.Fn
> import Network.Wai (Response, Application)
> import Network.Wai.Handler.Warp (run)

These import the libraries we'll need for the website: Fn, WAI (Web Application
Interface), and Warp. In Haskell, you can import modules from libraries with
`import` + the module name. Then you can optionally restrict the imports to just
certain types or functions (as in the second and third lines).

We're going to use Fn as the framework, Warp for the server, and Wai for
middleware (like it sounds, stuff in the middle between the framework and server).

> data Context = Context { req :: FnRequest }

Next we're making the context for our application. The idea is that
there's data we want to access when we're preparing a Response to send out to
a user. It might be in a database, a cache, environment variables, wherever! In
Fn, we wrap all those things up in a "Context". Here, the only information our
Context has is about the Request (we'll add other stuff in a later site).

Language/syntax note: This is called a data type. Data types in Haskell are
basically the coolest thing ever and you should read more about them (here).
But just bookmark that for later, because it's enough to know that this is
making a type called Context that has a constructor ALSO called Context, with
a field that has the type FnRequest. `req` is a function that takes a Context
and returns an FnRequest.

> instance RequestContext Context where
>  getRequest ctxt = req ctxt
>  setRequest ctxt newRequest = ctxt { req = newRequest }

These lines spell out for Fn how to get the request from the Context we made earlier.

> site :: Context -> IO Response
> site ctxt = route ctxt [ end ==> indexHandler ]
>                  `fallthrough` notFoundText "Page not found."

Our "site" is a way to take our Context, and turn it into a
Response. Don't worry too much about it right now because I'll go into much
more depth in the next part on routes!

> indexHandler :: Context -> IO (Maybe Response)
> indexHandler ctxt = okText "Welcome to my first Haskell website."

Hey, it's our first Handler! This handles a request by taking a context and giving back a response (again, we'll go into more detail on just what that `IO (Maybe Response)` there means in the next part). In this case, we're going to do the same thing every time someone requests this page -- just print a text message.

> main :: IO ()
> main = run 3000 waiApp
>
> waiApp :: Application
> waiApp = toWAI (Context defaultFnRequest) site

I said earlier that WAI is the middleware between our site and the server. This
is where we use WAI. Warp expects an app, so we use Fn's "toWAI" function to
turn an initial context and our `site` into an Application. `run 3000 waiApp`
means to run the Warp server with this app on port 3000.

Okay, so this site wasn't very exciting... let's learn more about routing and handlers!
