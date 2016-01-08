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

<span class="haskell-note">This is called a data type. Data types in Haskell are
basically the coolest thing ever and you should read more about them (here). This is
making a type called Context. You can build this data type by filling in "fields". Here
is a simpler example:

> data Dog = Dog { name :: String, age :: Int }

The data type is `Dog`. Its "constructor", also called `Dog`,
has the fields `name` and `age`. `name` has the type `String` and `age` has the type
`Int`. These types tell you what values you can use in the field. For example:

> fido :: Dog -- read, "`fido` has the type `Dog`"
> fido = Dog "Fido" 3

You "fill in" the fields in the same order listed. You can also use the names of
the fields like this:

> rex :: Dog
> rex = Dog { name = "Rex", age = 3 }

Try describing your own dog below!

> -- What's a good name for a dog? How old is the dog? 
>
>

How old is a dog? We can create a function to find out!

> dogsAge :: Dog -> Int
> dogsAge dog = age dog


You can also create NEW records based on ones you already have. So maybe the
info about "rex" above is out of date!

> updatedRex :: Dog
> updatedRex = rex { age = 4 }

This creates a new `Dog` based on our old `Dog`. Why not just change `rex` itself?

> -- rex = rex { age = 4 }

I've commented that out because it will cause an error! In Haskell, you can't
"mutate" data. `rex` is `rex` and will always be `rex`.

</span>

> instance RequestContext Context where
>  getRequest ctxt = req ctxt
>  setRequest ctxt newRequest = ctxt { req = newRequest }
  
These lines make `Context` an "instance" of `RequestContext`. Fn uses these
functions, `getRequest` and `setRequest`, to help us respond to requests.

<span class="haskell-note"> `RequestContext` is a typeclass. By using a typeclass,
we have the freedom to make a `Context`, a `Ctxt`, a `MyAwesomeRequestWrapperType`
-- whatever type we want to carry around whatever data we want! As long as we can
get an FnRequest OUT of it and put a new FnRequest INTO it, Fn will be able to use
our type. Let's look at another example!

> data PizzaJungle = Totoro { request :: FnRequest,
>                             mothersMaidenName :: String,
>                             isAmbidextrous :: Bool }
>
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
>                                secretKey :: String }

How could you write the `RequestContext` instance for `AppContext`?

> -- uncomment the next lines by deleting the "--"s when you have a solution!
> -- instance RequestContext AppContext where
> --   getRequest ????
> --   setRequest ????

(Need help?)

Okay, so back to `Context`. How do we actually use that?

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

Exercises
---------

Try 




Okay, so this site wasn't very exciting... let's learn more about routing and handlers!
