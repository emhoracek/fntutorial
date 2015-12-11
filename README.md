
This is a tutorial for the Haskell web framework Fn. It's designed to give
web developers an idea of how Haskell web development works. It won't
teach you *everything* you need to know, but I hope it'll give you a place to
start and an idea of just how fun it is! You don't have to know any Haskell to
begin, but you'll want to know how to use git and command line. 

To get started, clone this repository:

`git clone blah`

You'll need to install Stack, which installs the Haskell compiler and all the
libraries and modules you'll need. Just follow the directions for your operating
system [here](http://docs.haskellstack.org/en/stable/README.html).

Make sure "./local/bin" is in your $PATH, so your OS can find everything
Stack installs. (augh bad wording)

Then change to the `fntutorial` directory, and run `stack build`. This will download and install everything! It will take a very long time if you haven't used Stack before. Be patient. Have some tea/coffee/beverage of choice.

(If you have any problems building, let me know so I can help!)

Once the build is finished, run `stack exec site1` and visit [localhost:8000](http://localhost:8000) to see a "Hello world" Haskell site running on your machine!

Each site's source code contains a part of the tutorial. Read each "*.lhs" file's source to learn how to make Haskell sites with Fn, WAI, and Warp!

* "Site1.lhs" - `stack exec site1` -- a "Hello world!" site
* "Site2.lhs" - `stack exec site2` -- how do routes and handlers work? (not done yet!)
*  ...more to come!

(I will add more resources for how to learn more here)
