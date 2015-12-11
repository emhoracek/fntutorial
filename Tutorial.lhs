Fn Tutorial

Fn is a simple web framework build in Haskell.

dpb was trying to explain Snap, one of the more established frameworks, to me at work one day, and realized it's a little bit complicated. So he built Fn as a simplification. I think it's pretty neat! It's a little bit like Ruby's Sinatra.

This tutorial doesn't assume much knowledge of Haskelll. It doesn't teach you to build anything complicated or impressive, but it does make a website!

The first thing to know about Haskell is that everything in Haskell has a TYPE. A type is sort of like the shape of the data.

The second thing to know is that everything you DO in Haskell is a function. Functions also have Types.

>

So programming in Haskell is often a matter of figuring out what the type of your input is and what the type of your output is, and then what functions you need to chain together to change one type to the other.

There are types like Text and Integers just like in other programming languages. You can also make your own types by combining simpler types.

The third thing to know is that we won't have any "mutable state" in our program. Any data we need, we have to either start with it or we have to acquire it and then pass to from function to function.

One special sort of data we need is a Request. A Request is information about what the user wants to do. The request may be different every time someone visits our site. SO we need some way of getting a request and then looking it up whenever we need to.
