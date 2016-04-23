
This is a tutorial for the Haskell web framework Fn. It's designed to give
web developers an idea of how Haskell web development works. It won't
teach you *everything* you need to know, but I hope it'll give you a place to
start and an idea of just how fun it is!

To get started, you need to know [how to use git](git@github.com:emhoracek/fntutorial.git)
and some [command line basics](git@github.com:emhoracek/fntutorial.git).

You also want to know a little Haskell:

  * What are types and functions in Haskell?
  * How do you define a data type?
  * How do you make a data type an instance of a type class?
  * What the Maybe type?
  * How do you do stuff "in IO"?

[Learn you a Haskell](http://learnyouahaskell.com/) is one way to learn these basics,
and it's free online! You can also try the [Haskell wikibook](https://en.wikibooks.org/wiki/Haskell).

When you're ready to get started, clone this repository:

`git clone https://github.com/emhoracek/fntutorial.git`

You'll also need to install Stack, which installs the Haskell compiler and all the
libraries and modules you'll need. Just follow the directions for your operating
system [here](http://docs.haskellstack.org/en/stable/README.html).

Make sure "~/.local/bin" is in your $PATH, so your OS can find everything
Stack installs.

Then change to the `fntutorial` directory, and run `stack build`. This will download and install everything! It will take a very long time if you haven't used Stack before. Be patient. Have some tea/coffee/beverage of choice.

(If you have any problems building, let me know so I can help!)

Once the build is finished, run `stack exec site1` and visit [localhost:3000](http://localhost:8000) to see a "Hello world" Haskell site running on your machine! Type CTRL-C to quit the server when you're done.

Each site's source code contains a part of the tutorial. Read each "*.lhs" file's source to learn how to make Haskell sites with Fn, WAI, and Warp!

* "Site1.lhs" - `stack exec site1` -- a "Hello world!" site
* "Site2.lhs" - `stack exec site2` -- how do routes and handlers work? (not done yet!)
*  ...more to come!

[Start the tutorial!](http://fnhaskell.com/tutorial/Site1.html)
